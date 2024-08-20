{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module RuneScape.Cache.DiskStore
  ( -- * Disk store
    DiskStore
    -- ** Unsafe construction
  , AllocateDiskStoreError (..)
  , unsafeAllocateDiskStore
  , releaseDiskStore

    -- * Read
    -- ** Blocks
  , ReadBlockError (..)
  , readBlock
    -- ** Index file entries
  , ReadIndexEntryError (..)
  , readIndexEntry
    -- ** Containers
  , ReadContainerError (..)
  , readContainer
    -- ** Archive settings
  , ReadArchiveSettingsError (..)
  , readArchiveSettings
    -- ** Groups
  , ReadGroupError (..)
  , readGroup
    -- ** Files
  , ReadFileError (..)
  , readFile
  ) where

import Control.Exception
  ( IOException )
import Control.Monad
  ( filterM, when )
import Control.Monad.IO.Class
  ( liftIO )
import Control.Monad.Trans.Except
  ( ExceptT )
import Control.Monad.Trans.Except.Extra
  ( bimapExceptT, catchIOExceptT, firstExceptT, left, newExceptT, right )
import qualified Crypto.Cipher.Xtea as Xtea
import Data.Bifunctor
  ( second )
import Data.Binary.Get
  ( ByteOffset )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as BS
import Data.Conduit
  ( ConduitT, runConduit, yield, (.|) )
import Data.Conduit.Binary
  ( sourceHandleRangeWithBuffer )
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Serialization.Binary.Extra
  ( conduitGetEither )
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra
  ( third )
import Data.Word
  ( Word16, Word32 )
import Data.Word.Word24
  ( Word24 )
import Path
  ( Dir
  , SomeBase (..)
  , fromSomeDir
  , fromSomeFile
  , parseRelFile
  , relfile
  , (</>)
  )
import qualified Path as Path
import Prelude hiding
  ( readFile )
import RuneScape.Cache.Archive
  ( ArchiveId (..), settingsArchiveId )
import RuneScape.Cache.Archive.Settings
  ( ArchiveSettings (..), getArchiveSettings )
import RuneScape.Cache.Block
  ( Block
  , BlockHeaderType
  , BlockNumber (..)
  , archiveId
  , blockData
  , blockDataSize
  , blockSize
  , fromGroupId
  , getBlock
  , groupId
  , nextBlockNumber
  , relativeBlockNumber
  )
import RuneScape.Cache.Container
  ( Container (..)
  , DecryptAndDecompressError
  , decryptAndDecompress
  , getContainer
  )
import RuneScape.Cache.File
  ( File, FileId )
import RuneScape.Cache.Group
  ( Group (..), getGroup )
import RuneScape.Cache.Group.Id
  ( GroupId (..), SmallGroupId (..), toWord32 )
import RuneScape.Cache.Index
  ( IndexEntry (..), getIndexEntry, indexEntrySize )
import Safe.Numeric
  ( (*%), (+%), (-!) )
import System.Directory
  ( doesDirectoryExist, doesFileExist )
import System.IO
  ( Handle, IOMode (..), hClose, hFileSize, openFile )

-- | RuneScape cache resources loaded from disk.
--
-- This cache store supports the traditional RuneScape 2 and OldSchool
-- RuneScape cache layout on the filesystem. That is, @main_file_cache.dat2@
-- and @main_file_cache.idx*@ files within a single cache directory on the
-- filesystem.
data DiskStore = DiskStore
  { -- | Path to the root directory of the RuneScape cache.
    dsRootPath :: !(SomeBase Dir)
  , -- | Main data file.
    --
    -- Ensure that you appropriately 'hSeek' on this 'Handle' before utilizing
    -- it.
    dsDataFile :: !Handle
    -- | Master index file (file @main_file_cache.idx255@).
    --
    -- The master index file contains references to 'ArchiveSettings' in the
    -- main cache data file. This is a pertinent file as 'ArchiveSettings'
    -- values contain important metadata about 'Archive's, 'Group's, and
    -- 'File's in the cache. This metadata is required to properly deserialize
    -- the aforementioned structure from the main cache data file.
    --
    -- Technically, the master index file is structured just like any other
    -- index file ('dsIndexFiles') but, because it is a \"special\" index file
    -- (as explained above), we're keeping it in a separate field.
    --
    -- Ensure that you appropriately 'hSeek' on this 'Handle' before utilizing
    -- it.
  , dsMasterIndexFile :: !Handle
  , -- | Index files.
    --
    -- Ensure that you appropriately 'hSeek' on these 'Handle's before
    -- utilizing them.
    dsIndexFiles :: !(Map ArchiveId Handle)
  }

-- | Error allocating resources for a 'DiskStore'.
data AllocateDiskStoreError
  = -- | Provided RuneScape cache directory does not exist.
    AllocateDiskStoreCacheDirectoryDoesNotExistError !(SomeBase Dir)
  | -- | Main data file does not exist in the cache directory.
    AllocateDiskStoreDataFileDoesNotExistError !(SomeBase Path.File)
  | -- | Master\/settings index file does not exist in the cache directory.
    AllocateDiskStoreMasterIndexFileDoesNotExistError !(SomeBase Path.File)
  | -- | Expected archive index file does not exist in the cache directory.
    AllocateDiskStoreIndexFileDoesNotExistError !ArchiveId !(SomeBase Path.File)
  | AllocateDiskStoreIoExceptionError !IOException
  deriving stock (Show, Eq)

-- | Allocate resources for and construct a 'DiskStore'.
--
-- Note that it is unsafe to use this function without ensuring that
-- 'releaseDiskStore' is appropriately called.
unsafeAllocateDiskStore
  :: SomeBase Dir
  -- ^ Path to the root directory of the RuneScape cache.
  -> ExceptT AllocateDiskStoreError IO DiskStore
unsafeAllocateDiskStore path = do
  -- Check if the root directory of the cache exists
  liftIO (doesDirectoryExist (fromSomeDir path)) >>= \case
    False -> left (AllocateDiskStoreCacheDirectoryDoesNotExistError path)
    True -> pure ()

  -- Check if the main data file exists
  liftIO (doesFileExist (fromSomeFile dataFilePath)) >>= \case
    False -> left (AllocateDiskStoreDataFileDoesNotExistError dataFilePath)
    True -> pure ()

  -- Check if the master/settings index file exists (file 255)
  liftIO (doesFileExist (fromSomeFile masterIndexFilePath)) >>= \case
    False -> left (AllocateDiskStoreMasterIndexFileDoesNotExistError masterIndexFilePath)
    True -> pure ()

  -- Check which archive index files exist
  existingIndexFilePaths <- liftIO findIndexFilePaths

  dataFileHandle <-
    openFile (fromSomeFile dataFilePath) ReadMode
      `catchIOExceptT` AllocateDiskStoreIoExceptionError

  masterIndexFileHandle <-
    openFile (fromSomeFile masterIndexFilePath) ReadMode
      `catchIOExceptT` AllocateDiskStoreIoExceptionError

  indexFileHandles <-
    mapM
      (\(a, b) -> (,) a <$> openFile b ReadMode)
      (map (second fromSomeFile) existingIndexFilePaths)
        `catchIOExceptT` AllocateDiskStoreIoExceptionError

  pure DiskStore
    { dsRootPath = path
    , dsDataFile = dataFileHandle
    , dsMasterIndexFile = masterIndexFileHandle
    , dsIndexFiles = Map.fromList indexFileHandles
    }

  where
    dataFilePath :: SomeBase Path.File
    dataFilePath =
      let mainCacheFile = [relfile|main_file_cache.dat2|]
      in case path of
        Abs p -> Abs (p </> mainCacheFile)
        Rel p -> Rel (p </> mainCacheFile)

    masterIndexFilePath :: SomeBase Path.File
    masterIndexFilePath =
      let masterIndexFile = [relfile|main_file_cache.idx255|]
      in case path of
        Abs p -> Abs (p </> masterIndexFile)
        Rel p -> Rel (p </> masterIndexFile)

    mkIndexFilePath :: ArchiveId -> SomeBase Path.File
    mkIndexFilePath (ArchiveId aId) =
      case parseRelFile ("main_file_cache.idx" <> show aId) of
        Nothing -> error "impossible: could not parse index file path"
        Just indexFile ->
          case path of
            Abs p -> Abs (p </> indexFile)
            Rel p -> Rel (p </> indexFile)

    -- Index files with IDs from 0 to 254 (inclusive).
    --
    -- We purposely exclude 255 (the master index file) because a 'Handle' to
    -- it will be stored in a separate field from the other index files.
    possibleIndexFilePaths :: [(ArchiveId, SomeBase Path.File)]
    possibleIndexFilePaths =
      map (\a -> (a, mkIndexFilePath a)) [minBound .. maxBound -! 1]

    findIndexFilePaths :: IO [(ArchiveId, SomeBase Path.File)]
    findIndexFilePaths =
      filterM
        (doesFileExist . fromSomeFile . snd)
        possibleIndexFilePaths

-- | Release the resources associated with a 'DiskStore'.
releaseDiskStore :: DiskStore -> IO ()
releaseDiskStore ds = do
  hClose dsDataFile
  hClose dsMasterIndexFile
  mapM_ hClose dsIndexFiles
  where
    DiskStore
      { dsDataFile
      , dsMasterIndexFile
      , dsIndexFiles
      } = ds

-- | Error reading a 'Block'.
data ReadBlockError
  = -- | Integer overflow error was encountered when attempting multiplication.
    ReadBlockMultiplicationOverflowError
      -- | Left-hand side.
      !Word32
      -- | Right-hand side.
      !Word32
  | -- | Integer overflow error was encountered when attempting addition.
    ReadBlockAdditionOverflowError
      -- | Left-hand side.
      !Word32
      -- | Right-hand side.
      !Word32
  | -- | Block out of bounds error.
    ReadBlockOutOfBoundsError
      -- | Block number that was out of bounds.
      !BlockNumber
  | -- | Error deserializing a 'Block'.
    ReadBlockGetBlockError !(ByteString, ByteOffset, String)
  | ReadBlockIoExceptionError !IOException
  deriving stock (Show, Eq)

-- | Read a 'Block' from the cache.
readBlock :: DiskStore -> BlockHeaderType -> BlockNumber -> ExceptT ReadBlockError IO Block
readBlock ds bht blockNum@(BlockNumber bn) = do
  -- Check that the block is within the data file
  dataFileSize <- hFileSize dsDataFile `catchIOExceptT` ReadBlockIoExceptionError
  blockOffset <- calculateBlockOffset
  endOfBlockOffset <-
    case blockOffset +% blockSize of
      Left _ -> left (ReadBlockAdditionOverflowError blockOffset blockSize)
      Right x -> pure x
  when (fromIntegral endOfBlockOffset > dataFileSize) $
    left (ReadBlockOutOfBoundsError blockNum)

  bimapExceptT ReadBlockGetBlockError third
    . newExceptT
    . runConduit
    $ sourceConduit blockOffset
      .| conduitGetEither (getBlock bht)
      .| C.headDef (error "impossible: no blocks in the stream")
  where
    DiskStore
      { dsDataFile
      } = ds

    calculateBlockOffset :: Monad m => ExceptT ReadBlockError m Word32
    calculateBlockOffset =
      let blockNumberW32 :: Word32
          blockNumberW32 = fromIntegral bn
      in case blockSize *% blockNumberW32 of
        Left _ -> left (ReadBlockMultiplicationOverflowError blockSize blockNumberW32)
        Right x -> pure x

    sourceConduit :: Word32 -> ConduitT i ByteString IO ()
    sourceConduit blockOffset =
      sourceHandleRangeWithBuffer
        dsDataFile
        (Just $ fromIntegral blockOffset)
        (Just $ fromIntegral blockSize)
        (fromIntegral blockSize)

-- | Error reading an 'IndexEntry' from the cache.
data ReadIndexEntryError
  = -- | Error finding an index file.
    ReadIndexEntryIndexFileNotFoundError !ArchiveId
  | -- | Integer overflow error was encountered when attempting multiplication.
    ReadIndexEntryMultiplicationOverflowError
      -- | Left-hand side.
      !Word32
      -- | Right-hand side.
      !Word32
  | -- | Integer overflow error was encountered when attempting addition.
    ReadIndexEntryAdditionOverflowError
      -- | Left-hand side.
      !Word32
      -- | Right-hand side.
      !Word32
  | -- | Index entry out of bounds error.
    ReadIndexEntryOutOfBoundsError
      -- | Byte offset that was out of bounds.
      !Word32
  | -- | Error deserializing an index entry.
    ReadIndexEntryGetIndexEntryError !(ByteString, ByteOffset, String)
  | ReadIndexEntryIoExceptionError !IOException
  deriving stock (Show, Eq)

-- | Read an 'IndexEntry' from the cache.
--
-- Specifically, this 'IndexEntry' is read from the appropriate index file
-- (which is located via the provided 'ArchiveId').
--
-- The returned 'IndexEntry' points to the first data 'Block' of the requested
-- 'Group' in the cache's main data file.
readIndexEntry
  :: DiskStore
  -- ^ Disk store.
  -> ArchiveId
  -- ^ Archive identifier (i.e. index file identifier).
  -> GroupId
  -- ^ Group identifier (i.e. which group within the archive).
  -> ExceptT ReadIndexEntryError IO IndexEntry
readIndexEntry ds aId gId = do
  indexFileHandle <-
    case aId == settingsArchiveId of
      True -> right dsMasterIndexFile
      False ->
        case Map.lookup aId dsIndexFiles of
          Nothing -> left (ReadIndexEntryIndexFileNotFoundError aId)
          Just h -> right h

  -- Check that the entry is within the index file
  indexFileSize <- hFileSize indexFileHandle `catchIOExceptT` ReadIndexEntryIoExceptionError
  indexEntryOffset <- calculateIndexEntryOffset
  endOfIndexEntryOffset <-
    case indexEntryOffset +% indexEntrySize of
      Left _ -> left (ReadIndexEntryAdditionOverflowError indexEntryOffset indexEntrySize)
      Right x -> pure x
  when (fromIntegral endOfIndexEntryOffset > indexFileSize) $
    left (ReadIndexEntryOutOfBoundsError indexEntryOffset)

  bimapExceptT ReadIndexEntryGetIndexEntryError third
    . newExceptT
    . runConduit
    $ mkSourceConduit indexFileHandle indexEntryOffset
      .| conduitGetEither getIndexEntry
      .| C.headDef (error "impossible: no index entries in the stream")
  where
    DiskStore
      { dsMasterIndexFile
      , dsIndexFiles
      } = ds

    calculateIndexEntryOffset :: Monad m => ExceptT ReadIndexEntryError m Word32
    calculateIndexEntryOffset =
      let groupIdW32 :: Word32
          groupIdW32 = toWord32 gId
      in case groupIdW32 *% indexEntrySize of
        Left _ -> left (ReadIndexEntryMultiplicationOverflowError groupIdW32 indexEntrySize)
        Right x -> pure x

    mkSourceConduit :: Handle -> Word32 -> ConduitT i ByteString IO ()
    mkSourceConduit handle indexEntryOffset =
      sourceHandleRangeWithBuffer
        handle
        (Just $ fromIntegral indexEntryOffset)
        (Just $ fromIntegral indexEntrySize)
        (fromIntegral indexEntrySize)

-- | Error reading a 'Container' from the cache.
data ReadContainerError
  = -- | Error reading an index entry.
    ReadContainerReadIndexEntryError !ReadIndexEntryError
  | -- | Error reading a block.
    ReadContainerReadBlockError !ReadBlockError
  | -- | Actual size of data is less than the expected size.
    ReadContainerInvalidDataSizeError
      -- | Expected number of bytes to be read.
      !Word24
      -- | Actual number of bytes read.
      !Int
  | -- | Chain of data blocks does not end, even after reading all of the
    -- expected bytes.
    ReadContainerExpectedEndOfBlockChainError
      -- | First block in the invalid chain.
      !BlockNumber
      -- | Block that was expected to be the last in the chain, but it wasn't.
      !BlockNumber
  | -- | Integer overflow error was encountered when attempting to add an
    -- offset to a relative block number.
    ReadContainerRelativeBlockNumberOverflowError
      -- | Relative block number.
      !Word16
      -- | Offset.
      !Word16
  | -- | Relative block numbers aren't sequential.
    ReadContainerInvalidRelativeBlockNumberError
      -- | Invalid block.
      !BlockNumber
      -- | Expected relative block number.
      !Word16
      -- | Actual relative block number.
      !Word16
  | -- | Block has an invalid group ID.
    ReadContainerInvalidGroupIdError
      -- | Invalid block.
      !BlockNumber
      -- | Expected group ID.
      !GroupId
      -- | Actual group ID.
      !GroupId
  | -- | Block has an invalid archive ID.
    ReadContainerInvalidArchiveIdError
      -- | Invalid block.
      !BlockNumber
      -- | Expected archive ID.
      !ArchiveId
      -- | Actual archive ID.
      !ArchiveId
  | -- | Error deserializing a 'Container'.
    ReadContainerGetContainerError !(ByteString, ByteOffset, String)
  deriving stock (Show, Eq)

-- | Read raw container data from the cache.
--
-- Note that this function will follow the 'Block' chain and read all of the
-- specified container's data.
--
-- This is an internal function that isn't intended for export.
readContainerByIndexEntry
  :: DiskStore
  -- ^ Disk store.
  -> ArchiveId
  -- ^ Archive identifier. Used for validation of the blocks being read.
  -> GroupId
  -- ^ Group identifier. Used for validation of the blocks being read.
  -> IndexEntry
  -> ExceptT ReadContainerError IO ByteString
readContainerByIndexEntry ds aId gId ie = go BS.empty 0 ieBlockNumber ieDataSizeW32
  where
    IndexEntry
      { ieDataSize
      , ieBlockNumber
      } = ie

    bht :: BlockHeaderType
    bht = fromGroupId gId

    ieDataSizeW32 :: Word32
    ieDataSizeW32 = fromIntegral ieDataSize

    go :: ByteString -> Word16 -> BlockNumber -> Word32 -> ExceptT ReadContainerError IO ByteString
    go !acc _ (BlockNumber 0) 0 = pure acc
    go !acc _ (BlockNumber 0) _ = left $ ReadContainerInvalidDataSizeError ieDataSize (BS.length acc)
    go _ _ bn 0 = left $ ReadContainerExpectedEndOfBlockChainError ieBlockNumber bn
    go !acc expectedRelBn bn remainingBytes = do
      let remainingBytesI :: Int
          remainingBytesI = fromIntegral remainingBytes

      block <- firstExceptT ReadContainerReadBlockError (readBlock ds bht bn)

      -- Assert that the relative block number is what we expect.
      let relativeBlockNum = relativeBlockNumber block
      nextRelativeBlockNum <-
        case relativeBlockNum +% 1 of
          Left _ -> left (ReadContainerRelativeBlockNumberOverflowError relativeBlockNum 1)
          Right x -> pure x
      when (expectedRelBn /= relativeBlockNum) $
        left (ReadContainerInvalidRelativeBlockNumberError bn expectedRelBn relativeBlockNum)

      -- Assert that the group ID is what we expect.
      let actualGroupId = groupId block
      when (gId /= actualGroupId) $
        left (ReadContainerInvalidGroupIdError bn gId actualGroupId)

      -- Assert that the archive ID is what we expect.
      let actualArchiveId = archiveId block
      when (aId /= actualArchiveId) $
        left (ReadContainerInvalidArchiveIdError bn aId actualArchiveId)

      let bDataSz = blockDataSize bht
          bData = blockData block
          nextBlockNum = nextBlockNumber block
      if remainingBytes < bDataSz
        then go (acc <> BS.take remainingBytesI bData) nextRelativeBlockNum nextBlockNum 0
        else go (acc <> bData) nextRelativeBlockNum nextBlockNum (remainingBytes - bDataSz)

-- | Read a 'Container' from the cache.
--
-- Note that this function will follow the 'Block' chain and read all of the
-- specified container's data.
readContainer
  :: DiskStore
  -- ^ Disk store.
  -> ArchiveId
  -- ^ Archive identifier (i.e. index file identifier).
  -> GroupId
  -- ^ Group identifier (i.e. which group within the archive).
  -> ExceptT ReadContainerError IO Container
readContainer ds aId gId = do
  ie <- firstExceptT ReadContainerReadIndexEntryError (readIndexEntry ds aId gId)
  raw <- readContainerByIndexEntry ds aId gId ie
  bimapExceptT ReadContainerGetContainerError third
    . newExceptT
    . runConduit
    $ yield raw
      .| conduitGetEither getContainer
      .| C.headDef (error "impossible: no containers in the stream")

-- | Error reading archive settings from the cache.
data ReadArchiveSettingsError
  = -- | Error reading a container.
    ReadArchiveSettingsReadContainerError !ReadContainerError
  | -- | Error decompressing and decrypting archive settings.
    ReadArchiveSettingsDecryptAndDecompressError !DecryptAndDecompressError
  | -- | Error deserializing archive settings.
    ReadArchiveSettingsGetArchiveSettingsError !(ByteString, ByteOffset, String)
  deriving stock (Show, Eq)

-- | Read archive settings from the cache.
readArchiveSettings
  :: DiskStore
  -> ArchiveId
  -> ExceptT ReadArchiveSettingsError IO ArchiveSettings
readArchiveSettings ds (ArchiveId aId) = do
  let archiveIdAsGroupId :: GroupId
      archiveIdAsGroupId = GroupIdSmall (SmallGroupId $ fromIntegral aId)
  container <- firstExceptT ReadArchiveSettingsReadContainerError (readContainer ds settingsArchiveId archiveIdAsGroupId)
  settingsData <- firstExceptT ReadArchiveSettingsDecryptAndDecompressError (decryptAndDecompress Nothing container)
  bimapExceptT ReadArchiveSettingsGetArchiveSettingsError third
    . newExceptT
    . runConduit
    $ yield settingsData
      .| conduitGetEither getArchiveSettings
      .| C.headDef (error "impossible: no results in the stream")

-- | Error reading a group from the cache.
data ReadGroupError
  = -- | Error reading archive settings.
    ReadGroupReadArchiveSettingsError !ReadArchiveSettingsError
  | -- | Requested group does not exist within the specified archive.
    ReadGroupDoesNotExistError !ArchiveId !GroupId
  | -- | Error reading a container.
    ReadGroupReadContainerError !ReadContainerError
  | -- | Error decompressing and decrypting a group.
    ReadGroupDecryptAndDecompressError !DecryptAndDecompressError
  | -- | Error deserializing a group.
    ReadGroupGetGroupError !(ByteString, ByteOffset, String)
  deriving stock (Show, Eq)

-- | Read a group from the cache.
--
-- Note that this function will follow the 'Block' chain and read all of the
-- specified group container's data. Following that, the container will be
-- decrypted and decompressed. Finally, the files within the group's data will
-- be unpacked.
readGroup
  :: DiskStore
  -- ^ Disk store.
  -> ArchiveId
  -- ^ Archive identifier (i.e. index file identifier).
  -> GroupId
  -- ^ Group identifier (i.e. which group within the archive).
  -> Maybe Xtea.SymmetricKey
  -- ^ XTEA symmetric key to decrypt the group data.
  -> ExceptT ReadGroupError IO Group
readGroup ds aId gId k = do
  ArchiveSettings{ asGroupSettings } <- firstExceptT ReadGroupReadArchiveSettingsError (readArchiveSettings ds aId)
  settings <-
    case Map.lookup gId asGroupSettings of
      Just x -> pure x
      Nothing -> left (ReadGroupDoesNotExistError aId gId)
  container <- firstExceptT ReadGroupReadContainerError (readContainer ds aId gId)
  groupData <- firstExceptT ReadGroupDecryptAndDecompressError (decryptAndDecompress k container)
  bimapExceptT ReadGroupGetGroupError third
    . newExceptT
    . runConduit
    $ yield groupData
      .| conduitGetEither (getGroup settings $ fromIntegral $ BS.length groupData)
      .| C.headDef (error "impossible: no groups in the stream")

-- | Error reading a file from the cache.
data ReadFileError
  = -- | Error reading a group.
    ReadFileReadGroupError !ReadGroupError
  | -- | Requested file does not exist.
    ReadFileDoesNotExistError !ArchiveId !GroupId !FileId
  deriving stock (Show, Eq)

-- | Read a file from the cache.
readFile
  :: DiskStore
  -- ^ Disk store.
  -> ArchiveId
  -- ^ Archive identifier (i.e. index file identifier).
  -> GroupId
  -- ^ Group identifier (i.e. which group within the archive).
  -> FileId
  -- ^ File identifier (i.e. which file within the group).
  -> Maybe Xtea.SymmetricKey
  -- ^ XTEA symmetric key to decrypt the file.
  -> ExceptT ReadFileError IO File
readFile ds aId gId fId k = do
  Group { gFiles } <- firstExceptT ReadFileReadGroupError (readGroup ds aId gId k)
  case Map.lookup fId gFiles of
    Just x -> pure x
    Nothing -> left (ReadFileDoesNotExistError aId gId fId)
