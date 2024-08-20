module RuneScape.Cache.Archive.Settings
  ( Protocol (..)
  , getProtocol

  , Version (..)
  , getVersion

  , Flag (..)
  , toBitmask

  , Flags (..)
  , hasFlag
  , toWord8
  , fromWord8

  , ArchiveSettings (..)
  , getArchiveSettings
  ) where

import Control.Monad
  ( replicateM )
import Crypto.Hash
  ( Digest, Whirlpool, digestFromByteString, hashDigestSize )
import Data.Binary.Get
  ( Get, getByteString, getWord16be, getWord32be, getWord8 )
import Data.Binary.Get.Delta
  ( getDeltaEncodedValues )
import Data.Bits
  ( xor, (.&.) )
import qualified Data.Foldable as F
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import Data.Set
  ( Set )
import qualified Data.Set as Set
import Data.Traversable
  ( for )
import Data.Word
  ( Word32, Word8 )
import Data.Word.Smart
  ( getSmartWordbe )
import qualified Data.Word.Smart as Smart
import Prelude
import RuneScape.Cache.File
  ( FileId (..) )
import RuneScape.Cache.File.Settings
  ( FileSettings (..) )
import RuneScape.Cache.Group.Id
  ( GroupId )
import qualified RuneScape.Cache.Group.Id as Group
import RuneScape.Cache.Group.Settings
  ( CompressedSize
  , GroupSettings (..)
  , UncompressedSize
  , getCompressedSize
  , getUncompressedSize
  )
import qualified RuneScape.Cache.Group.Settings as Group.Settings

-- | Archive settings protocol.
data Protocol
  = -- | Archive settings structure is not versioned.
    UnversionedProtocol
  | -- | Archive settings structure is versioned.
    VersionedProtocol
  | -- | Archive settings structure is versioned and utilizes a larger
    -- \"smart\" integer type for some of its fields.
    VersionedSmartProtocol
  deriving stock (Show, Eq)

getProtocol :: Get Protocol
getProtocol = do
  x <- getWord8
  case x of
    5 -> pure UnversionedProtocol
    6 -> pure VersionedProtocol
    7 -> pure VersionedSmartProtocol
    _ -> fail $ "Invalid protocol: " <> show x

-- | Archive settings version.
newtype Version = Version
  { unVersion :: Word32 }
  deriving stock (Show, Eq)

getVersion :: Get Version
getVersion = Version <$> getWord32be

-- | Archive settings flag.
--
-- These flags indicate what information is available in an archive's
-- settings\/metadata.
data Flag
  = -- | Group and file name hashes are available for the archive.
    --
    -- Note that, because only hashes are available, it is necessary to know
    -- the plaintext names of the groups or files that one wouldlike to
    -- lookup.
    NameHashesFlag
  | -- | Whirlpool digests of compressed group data are available for the
    -- archive.
    WhirlpoolDigestsFlag
  | -- | Compressed and uncompressed sizes (in bytes) of group data are
    -- available for the archive.
    SizesFlag
  | -- | CRC-32 checksums of uncompressed group data are available for the
    -- archive.
    UncompressedChecksumsFlag
  deriving stock (Show, Eq, Ord, Bounded, Enum)

-- | Convert an 'Flag' to its bitmask.
toBitmask :: Flag -> Word8
toBitmask flag =
  case flag of
    NameHashesFlag -> 0x01
    WhirlpoolDigestsFlag -> 0x02
    SizesFlag -> 0x04
    UncompressedChecksumsFlag -> 0x08

-- | Collection of archive settings flags.
newtype Flags = Flags
  { unFlags :: Set Flag }
  deriving stock (Show, Eq)

-- | Check whether an 'Flags' has a specified 'Flag'.
hasFlag :: Flags -> Flag -> Bool
hasFlag flags flag = flag `Set.member` unFlags flags

-- | Convert an 'Flags' to a 'Word8'.
toWord8 :: Flags -> Word8
toWord8 = F.foldl' (\acc flag -> acc `xor` toBitmask flag) 0x0000 . unFlags

-- | Convert a 'Word8' to an 'Flags'.
fromWord8 :: Word8 -> Flags
fromWord8 b = F.foldl' f (Flags Set.empty) [minBound .. maxBound]
  where
    f :: Flags -> Flag -> Flags
    f (Flags accFlags) flag =
      if b .&. toBitmask flag > 0
        then Flags (Set.insert flag accFlags)
        else Flags accFlags

getFlags :: Get Flags
getFlags = fromWord8 <$> getWord8

-- | Archive settings.
--
-- This contains metadata about an 'Archive' and the 'Group's contained within it.
data ArchiveSettings = ArchiveSettings
  { asProtocol :: !Protocol
  , asVersion :: !(Maybe Version)
  , -- | Flags indicating what kinds of extra metadata are available.
    asFlags :: !Flags
  , -- | Metadata pertaining to all of the 'Group's belonging to this
    -- 'Archive'.
    asGroupSettings :: !(Map GroupId GroupSettings)
  } deriving stock (Show, Eq)

getArchiveSettings :: Get ArchiveSettings
getArchiveSettings = do
  protocol <- getProtocol
  mbVersion <-
    case protocol of
      UnversionedProtocol -> pure Nothing
      _ -> Just <$> getVersion
  flags <- getFlags
  groupSettings <- getGroupSettingsMap protocol flags
  pure ArchiveSettings
    { asProtocol = protocol
    , asVersion = mbVersion
    , asFlags = flags
    , asGroupSettings = groupSettings
    }

getGroupSettingsMap :: Protocol -> Flags -> Get (Map GroupId GroupSettings)
getGroupSettingsMap protocol flags = do
  groupCount <-
    case protocol of
      VersionedSmartProtocol -> Smart.toWord32 <$> getSmartWordbe
      _ -> fromIntegral <$> getWord16be
  let groupCountI :: Int
      groupCountI = fromIntegral groupCount
  groupIds <-
    map Group.fromWord32 <$>
      ( getDeltaEncodedValues groupCount $
          case protocol of
            VersionedSmartProtocol -> Smart.toWord32 <$> getSmartWordbe
            _ -> fromIntegral <$> getWord16be
      )
  mbNameHashes <-
    if hasFlag flags NameHashesFlag
      then Just <$> replicateM groupCountI getWord32be
      else pure Nothing
  compressedChecksums <- replicateM groupCountI getWord32be
  mbUncompressedChecksums <-
    if hasFlag flags UncompressedChecksumsFlag
      then Just <$> replicateM groupCountI getWord32be
      else pure Nothing
  mbWhirlpoolDigests <-
    if hasFlag flags WhirlpoolDigestsFlag
      then Just <$> replicateM groupCountI getWhirlpoolDigest
      else pure Nothing
  mbSizes <-
    if hasFlag flags SizesFlag
      then Just <$> replicateM groupCountI ((,) <$> getCompressedSize <*> getUncompressedSize)
      else pure Nothing
  versions <- replicateM groupCountI Group.Settings.getVersion
  fileSettings <- getFileSettingsMaps protocol flags groupCountI

  let paramTuples =
        zip8
          groupIds
          (mbListToListMbs groupCountI mbNameHashes)
          compressedChecksums
          (mbListToListMbs groupCountI mbUncompressedChecksums)
          (mbListToListMbs groupCountI mbWhirlpoolDigests)
          (mbListToListMbs groupCountI mbSizes)
          versions
          fileSettings
  pure (mkGroupSettingsMap paramTuples)
  where
    getWhirlpoolDigest :: Get (Digest Whirlpool)
    getWhirlpoolDigest = do
      -- Whirlpool digest size in bytes.
      let whirlpoolDigestSize :: Int
          whirlpoolDigestSize = hashDigestSize (error "Whirlpool" :: Whirlpool)
      bs <- getByteString whirlpoolDigestSize
      case digestFromByteString bs of
        Nothing -> fail "Invalid digest size"
        Just d -> pure d

    mkGroupSettings
      :: (GroupId, Maybe Word32, Word32, Maybe Word32, Maybe (Digest Whirlpool), Maybe (CompressedSize, UncompressedSize), Group.Settings.Version, Map FileId FileSettings)
      -> (GroupId, GroupSettings)
    mkGroupSettings (groupId, nameHash, compressedCsum, uncompressedCsum, whirlpoolDigest, sizes, version, fileSettings) =
      let gs =
            GroupSettings
              { gsId = groupId
              , gsNameHash = nameHash
              , gsCompressedChecksum = compressedCsum
              , gsUncompressedChecksum = uncompressedCsum
              , gsWhirlpoolDigest = whirlpoolDigest
              , gsSizes = sizes
              , gsVersion = version
              , gsFileSettings = fileSettings
              }
      in (groupId, gs)

    mkGroupSettingsMap
      :: [(GroupId, Maybe Word32, Word32, Maybe Word32, Maybe (Digest Whirlpool), Maybe (CompressedSize, UncompressedSize), Group.Settings.Version, Map FileId FileSettings)]
      -> Map GroupId GroupSettings
    mkGroupSettingsMap = Map.fromList . map mkGroupSettings

getFileSettingsMaps :: Protocol -> Flags -> Int -> Get [Map FileId FileSettings]
getFileSettingsMaps protocol flags groupCountI = do
  groupFileCounts <-
    replicateM groupCountI $
      case protocol of
        VersionedSmartProtocol -> Smart.toWord32 <$> getSmartWordbe
        _ -> fromIntegral <$> getWord16be
  groupFileIds <-
    for groupFileCounts $ \fileCount ->
      map FileId <$>
        ( getDeltaEncodedValues fileCount $
            case protocol of
              VersionedSmartProtocol -> Smart.toWord32 <$> getSmartWordbe
              _ -> fromIntegral <$> getWord16be
        )
  groupFileNameHashes <-
    for groupFileIds $ \fileIds ->
      let numFileIds = length fileIds
      in mbListToListMbs numFileIds <$>
        ( if hasFlag flags NameHashesFlag
            then Just <$> replicateM numFileIds getWord32be
            else pure Nothing
        )

  pure (mkFileSettingsMaps $ mkParams groupFileIds groupFileNameHashes)
  where
    mkFileSettings :: (FileId, Maybe Word32) -> (FileId, FileSettings)
    mkFileSettings (fileId, mbNameHash) =
      let fs =
            FileSettings
              { fsId = fileId
              , fsNameHash = mbNameHash
              }
      in (fileId, fs)

    mkFileSettingsMap :: [(FileId, Maybe Word32)] -> Map FileId FileSettings
    mkFileSettingsMap = Map.fromList . map mkFileSettings

    mkFileSettingsMaps :: [[(FileId, Maybe Word32)]] -> [Map FileId FileSettings]
    mkFileSettingsMaps = map mkFileSettingsMap

    mkParams :: [[FileId]] -> [[Maybe Word32]] -> [[(FileId, Maybe Word32)]]
    mkParams = go []
      where
        go :: [[(FileId, Maybe Word32)]] -> [[FileId]] -> [[Maybe Word32]] -> [[(FileId, Maybe Word32)]]
        go acc (x : xs) (y : ys) = go (zip x y : acc) xs ys
        go acc _ _ = reverse acc

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

mbListToListMbs :: Int -> Maybe [a] -> [Maybe a]
mbListToListMbs n Nothing = replicate n Nothing
mbListToListMbs _ (Just xs) = map Just xs

zipWith8
  :: (a->b->c->d->e->f->g->h->i)
  -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]
zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) =
  z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ = []

zip8
  :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
  -> [(a,b,c,d,e,f,g,h)]
zip8 = zipWith8 (,,,,,,,)
