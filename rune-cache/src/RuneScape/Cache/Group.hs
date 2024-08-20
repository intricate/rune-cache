module RuneScape.Cache.Group
  ( Group (..)
  , getGroup
  ) where

import Control.Monad
  ( replicateM )
import Data.Binary.Get
  ( Get
  , getByteString
  , getInt32be
  , getRemainingLazyByteString
  , getWord8
  , isEmpty
  , lookAhead
  , skip
  )
import Data.Binary.Get.Delta
  ( getDeltaEncodedValues )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import Data.Int
  ( Int32 )
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import Data.Traversable
  ( for )
import Data.Word
  ( Word32, Word8 )
import Prelude
import RuneScape.Cache.File
  ( File (..), FileId )
import RuneScape.Cache.Group.Settings
  ( GroupSettings (..) )
import Safe.Numeric
  ( (*%), (-%) )

-- | Group of files.
data Group = Group
  { -- | Files belonging to this group.
    gFiles :: !(Map FileId File)
  , -- | Group metadata.
    gSettings :: !GroupSettings
  } deriving stock (Show, Eq)

-- | Deserialize a 'Group' from decompressed and decrypted 'Container' data.
getGroup
  :: GroupSettings
  -- ^ Metadata about the group to be deserialized.
  -> Word32
  -- ^ Size (in bytes) of the decompressed and decrypted group data.
  -> Get Group
getGroup settings groupSize =
  case Map.size gsFileSettings of
    1 -> do
      -- If there is only one file in the group, then the group data /is/ the
      -- file data.
      fileId <-
        case Map.toList gsFileSettings of
          [(k, _)] -> pure k
          _ -> fail "impossible: Map of size 1 does not have exactly one element"
      fileData <- LBS.toStrict <$> getRemainingLazyByteString
      pure Group
        { gFiles =
            Map.singleton fileId
              File
                { fId = fileId
                , fData = fileData
                }
        , gSettings = settings
        }
    numFiles
      | numFiles < 1 -> fail "There are no files in the group."
      | otherwise -> do
          -- Get the chunk count from the end of the group data without
          -- consuming input.
          chunkCount <- getChunkCount

          -- Get the file chunk sizes from near the end of the group data
          -- without consuming input.
          fileChunkSizes <- getFileChunkSizes chunkCount

          -- Get the file chunks and then combine them to construct the full
          -- files.
          fileChunks <- getFileChunks fileChunkSizes
          let files = chunksToFiles fileChunks

          pure Group
            { gFiles = toFileMap files
            , gSettings = settings
            }
  where
    GroupSettings
      { gsFileSettings
      } = settings

    groupSizeI :: Int
    groupSizeI = fromIntegral groupSize

    validateFileChunkSizes :: [[Int32]] -> Maybe [[Word32]]
    validateFileChunkSizes = mapM (go [])
      where
        go :: [Word32] -> [Int32] -> Maybe [Word32]
        go acc [] = Just $ reverse acc
        go acc (x : xs)
          | x > 0 = go (fromIntegral x : acc) xs
          | otherwise = Nothing

    getChunkCount :: Get Word8
    getChunkCount = lookAhead $ do
      offset <-
        case groupSizeI -% 1 of
          Left _ -> fail "Invalid groupSize provided. Encountered underflow error after subtracting 1 from groupSize."
          Right x -> pure x
      skip offset
      b <- getWord8
      empty <- isEmpty
      case empty of
        True -> pure b
        False -> fail "Invalid groupSize provided. Input should be empty after groupSize bytes."

    getFileChunkSizes :: Word8 -> Get [[Word32]]
    getFileChunkSizes chunkCount = lookAhead $ do
      let chunkCountI :: Int
          chunkCountI = fromIntegral chunkCount

          fileCountI :: Int
          fileCountI = Map.size gsFileSettings

          fileCountW32 :: Word32
          fileCountW32 = fromIntegral fileCountI

      sizeOfFileChunkSizes <-
        -- Safe variant of `chunkCountI * fileCountI * 4`
        case chunkCountI *% fileCountI >>= (*%) 4 of
          Left _ -> fail "Encountered an overflow when calculating the total size of file chunk sizes."
          Right x -> pure x

      let sizeOfChunkCount = 1

      fileChunkSizesOffset <-
        -- Safe variant of `groupSizeI - sizeOfChunkCount - sizeOfFileChunkSizes`
        case groupSizeI -% sizeOfChunkCount >>= \x -> x -% sizeOfFileChunkSizes of
          Left _ -> fail "Encountered an underflow when calculating fileChunkSizesOffset."
          Right x -> pure x

      -- Skip to the offset where we can begin to deserialize the file chunk
      -- sizes.
      skip fileChunkSizesOffset

      fileChunkSizes <-
        validateFileChunkSizes
          <$> replicateM chunkCountI (getDeltaEncodedValues fileCountW32 getInt32be)

      case fileChunkSizes of
        Just x -> pure x
        Nothing -> fail "File chunk sizes are invalid. At least one of the sizes is less than or equal to 0."

    getFileChunks :: [[Word32]] -> Get [[ByteString]]
    getFileChunks fileChunkSizes =
      for fileChunkSizes $ \chunkSizes ->
        for chunkSizes $ \chunkSize ->
          getByteString (fromIntegral chunkSize)

    chunksToFiles :: [[ByteString]] -> [ByteString]
    chunksToFiles [] = error "impossible: chunksToFiles: empty list of chunks"
    chunksToFiles [c] = c
    chunksToFiles (c : cs) = go c cs
      where
        go :: [ByteString] -> [[ByteString]] -> [ByteString]
        go [] [] = error "impossible: chunksToFiles: empty list args"
        go _ [] = error "impossible: chunksToFiles: empty list of chunks"
        go xs [y] = zipWith (<>) xs y
        go xs (y : ys) = zipWith (<>) xs (go y ys)

    toFileMap :: [ByteString] -> Map FileId File
    toFileMap files = Map.fromList $ zipWith (\a b -> (a, File a b)) (Map.keys gsFileSettings) files
