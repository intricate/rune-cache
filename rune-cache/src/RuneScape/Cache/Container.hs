{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Optionally compressed and encrypted containers of data.
--
-- Containers can consist of either group data or archive settings data.
module RuneScape.Cache.Container
  ( -- * Container
    Container (..)
  , getContainer
  , DecryptAndDecompressError (..)
  , decryptAndDecompress

    -- ** Uncompressed container
  , UncompressedContainer (..)

    -- ** Compressed container
  , CompressedContainer (..)
  , MaybeEncryptedCompressedContainerData (..)
  , UnencryptedCompressedContainerData (..)
  ) where

import Control.Monad.Trans.Except
  ( ExceptT )
import Control.Monad.Trans.Except.Extra
  ( firstExceptT, hoistEither, left )
import qualified Crypto.Cipher.Xtea as Xtea
import Data.Bifunctor
  ( bimap, first )
import Data.Binary.Get
  ( ByteOffset, Get, getByteString, getWord32be, getWord8, runGetOrFail )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Tuple.Extra
  ( third )
import Data.Word
  ( Word32 )
import Prelude
import RuneScape.Cache.Compression
  ( CompressedData (..)
  , Compression (..)
  , CompressionType (..)
  , DecompressionError (..)
  , SomeDecompressionError (..)
  , decompress
  )
import RuneScape.Cache.Container.Encryption
  ( MaybeEncrypted (..), decrypt, toUnencryptedBytes )

------------------------------------------------------------------------------
-- Compression
------------------------------------------------------------------------------

-- | Raw compression type.
--
-- This is an internal type only to be used in parsing.
data RawCompressionType
  = -- | Uncompressed.
    RawCompressionTypeUncompressed
  | -- | @bzip2@ compression.
    RawCompressionTypeBzip2
  | -- | @gzip@ compression.
    RawCompressionTypeGzip
  | -- | LZMA compression.
    RawCompressionTypeLzma
  deriving stock (Show, Eq)

getRawCompressionType :: Get RawCompressionType
getRawCompressionType = do
  b <- getWord8
  case b of
    0 -> pure RawCompressionTypeUncompressed
    1 -> pure RawCompressionTypeBzip2
    2 -> pure RawCompressionTypeGzip
    3 -> pure RawCompressionTypeLzma
    _ -> fail $ "Invalid compression type: " <> show b

------------------------------------------------------------------------------
-- Decryption error
------------------------------------------------------------------------------

-- | Error decrypting and deserializing the container.
data DecryptContainerError
  = -- | Error decrypting the container.
    DecryptContainerDecryptionError !Xtea.DecryptionError
  | -- | Error deserializing the decrypted container data.
    DecryptContainerGetError !(LBS.ByteString, ByteOffset, String)
  deriving stock (Show, Eq)

------------------------------------------------------------------------------
-- Uncompressed container
------------------------------------------------------------------------------

-- | Container consisting of uncompressed and possibly encrypted group data.
data UncompressedContainer = UncompressedContainer
  { -- | Size of the group data (in bytes).
    ucDataSize :: !Word32
  , -- | Uncompressed group data (possibly encrypted).
    ucData :: !(MaybeEncrypted ByteString)
  } deriving stock (Show, Eq)

-- | Decrypt an 'UncompressedContainer' payload.
--
-- If 'Nothing' is provided for the key value, then it is assumed that the
-- container data is unencrypted, so no decryption will be performed.
decryptUncompressedContainer
  :: Maybe Xtea.SymmetricKey
  -> UncompressedContainer
  -> Either DecryptContainerError ByteString
decryptUncompressedContainer mbKey c =
  case mbKey of
    Just k -> first DecryptContainerDecryptionError (decrypt k ucData)
    Nothing -> pure (toUnencryptedBytes ucData)
  where
    UncompressedContainer
      { ucData
      } = c

------------------------------------------------------------------------------
-- Compressed container
------------------------------------------------------------------------------

-- | 'CompressedContainer' data that is unencrypted.
data UnencryptedCompressedContainerData c = UnencryptedCompressedContainerData
  { -- | Size of the group data (in bytes) before compression.
    uccdUncompressedDataSize :: !Word32
  , -- | Compressed group data.
    uccdData :: !(CompressedData c)
  } deriving stock (Show, Eq)

getUnencryptedCompressedContainerData
  :: CompressionType c
  -- ^ Compression type.
  -> Word32
  -- ^ Compressed data size.
  -> Get (UnencryptedCompressedContainerData c)
getUnencryptedCompressedContainerData ct compressedDataSize = do
  uncompressedSize <- getWord32be
  bs <- getByteString (fromIntegral compressedDataSize)
  pure UnencryptedCompressedContainerData
    { uccdUncompressedDataSize = uncompressedSize
    , uccdData =
        case ct of
          CompressionTypeBzip2 -> CompressedDataBzip2 bs
          CompressionTypeGzip -> CompressedDataGzip bs
          CompressionTypeLzma -> CompressedDataLzma bs
    }

-- | 'CompressedContainer' data that is possibly encrypted.
data MaybeEncryptedCompressedContainerData c where
  MaybeEncryptedBzip2ContainerData :: MaybeEncrypted ByteString -> MaybeEncryptedCompressedContainerData 'Bzip2
  MaybeEncryptedGzipContainerData :: MaybeEncrypted ByteString -> MaybeEncryptedCompressedContainerData 'Gzip
  MaybeEncryptedLzmaContainerData :: MaybeEncrypted ByteString -> MaybeEncryptedCompressedContainerData 'Lzma

deriving instance Show (MaybeEncryptedCompressedContainerData c)

deriving instance Eq (MaybeEncryptedCompressedContainerData c)

getMaybeEncryptedCompressedContainerData
  :: CompressionType c
  -- ^ Compression type.
  -> Word32
  -- ^ Length of compressed data
  -> Get (MaybeEncryptedCompressedContainerData c)
getMaybeEncryptedCompressedContainerData ct compressedSize = do
  let uncompressedSizeLen = 4
  bs <- MaybeEncrypted <$> getByteString (uncompressedSizeLen + (fromIntegral compressedSize))
  pure $ case ct of
    CompressionTypeBzip2 -> MaybeEncryptedBzip2ContainerData bs
    CompressionTypeGzip -> MaybeEncryptedGzipContainerData bs
    CompressionTypeLzma -> MaybeEncryptedLzmaContainerData bs

-- | Container consisting of compressed and possibly encrypted group data.
data CompressedContainer c = CompressedContainer
  { -- | Size of the group data (in bytes) after compression.
    ccCompressedDataSize :: !Word32
  , -- | Compressed group data (possibly encrypted).
    ccData :: !(MaybeEncryptedCompressedContainerData c)
  } deriving stock (Show, Eq)

-- | Decrypt a 'CompressedContainer' payload.
--
-- If 'Nothing' is provided for the key value, then it is assumed that the
-- container data is unencrypted, so no decryption will be performed.
decryptCompressedContainer
  :: Maybe Xtea.SymmetricKey
  -> CompressedContainer c
  -> Either DecryptContainerError (UnencryptedCompressedContainerData c)
decryptCompressedContainer mbKey c = do
  decrypted <-
    case mbKey of
      Just k -> first DecryptContainerDecryptionError (decrypt k mbEncryptedData)
      Nothing -> pure (toUnencryptedBytes mbEncryptedData)
  bimap DecryptContainerGetError third $
    runGetOrFail
      (getUnencryptedCompressedContainerData compressionType ccCompressedDataSize)
      (LBS.fromStrict decrypted)
  where
    CompressedContainer
      { ccCompressedDataSize
      , ccData
      } = c

    compressionType =
      case ccData of
        MaybeEncryptedBzip2ContainerData _ -> CompressionTypeBzip2
        MaybeEncryptedGzipContainerData _ -> CompressionTypeGzip
        MaybeEncryptedLzmaContainerData _ -> CompressionTypeLzma

    mbEncryptedData :: MaybeEncrypted ByteString
    mbEncryptedData =
      case ccData of
        MaybeEncryptedBzip2ContainerData e -> e
        MaybeEncryptedGzipContainerData e -> e
        MaybeEncryptedLzmaContainerData e -> e

------------------------------------------------------------------------------
-- General container
------------------------------------------------------------------------------

-- | Container consisting of optionally compressed and encrypted group data.
data Container
  = -- | Uncompressed container.
    ContainerUncompressed !UncompressedContainer
  | -- | @bzip2@-compressed container.
    ContainerCompressedBzip2 !(CompressedContainer 'Bzip2)
  | -- | @gzip@-compressed container.
    ContainerCompressedGzip !(CompressedContainer 'Gzip)
  | -- | LZMA-compressed container.
    ContainerCompressedLzma !(CompressedContainer 'Lzma)
  deriving stock (Show, Eq)

getContainer :: Get Container
getContainer = do
  compressionType <- getRawCompressionType
  compressedSize <- getWord32be
  case compressionType of
    RawCompressionTypeUncompressed -> do
      bs <- MaybeEncrypted <$> getByteString (fromIntegral compressedSize)
      pure $ ContainerUncompressed
        UncompressedContainer
          { ucDataSize = compressedSize
          , ucData = bs
          }
    RawCompressionTypeBzip2 -> do
      containerData <- getMaybeEncryptedCompressedContainerData CompressionTypeBzip2 compressedSize
      pure $ ContainerCompressedBzip2
        CompressedContainer
          { ccCompressedDataSize = compressedSize
          , ccData = containerData
          }
    RawCompressionTypeGzip -> do
      containerData <- getMaybeEncryptedCompressedContainerData CompressionTypeGzip compressedSize
      pure $ ContainerCompressedGzip
        CompressedContainer
          { ccCompressedDataSize = compressedSize
          , ccData = containerData
          }
    RawCompressionTypeLzma -> do
      containerData <- getMaybeEncryptedCompressedContainerData CompressionTypeLzma compressedSize
      pure $ ContainerCompressedLzma
        CompressedContainer
          { ccCompressedDataSize = compressedSize
          , ccData = containerData
          }

-- | Error decrypting and decompressing a 'Container'.
data DecryptAndDecompressError
  = -- | Decompression error.
    DecryptAndDecompressDecompressionError !SomeDecompressionError
  | -- | Decryption error.
    DecryptAndDecompressDecryptionError !DecryptContainerError
  | DecryptAndDecompressInvalidDecompressedDataSizeError
      -- | Expected size of the decompressed data
      !Word32
      -- | Actual size of the decompressed data.
      !Word32
  deriving stock (Show, Eq)

-- | Decrypt and decompress a 'Container'.
--
-- If 'Nothing' is provided for the key value, then it is assumed that the
-- container data is unencrypted, so no decryption will be performed.
decryptAndDecompress
  :: Maybe Xtea.SymmetricKey
  -> Container
  -> ExceptT DecryptAndDecompressError IO ByteString
decryptAndDecompress k c = do
  case c of
    ContainerUncompressed uc ->
      firstExceptT DecryptAndDecompressDecryptionError
        . hoistEither
        $ decryptUncompressedContainer k uc
    ContainerCompressedBzip2 cc -> decryptAndDecompressCompressedContainer cc
    ContainerCompressedGzip cc -> decryptAndDecompressCompressedContainer cc
    ContainerCompressedLzma cc -> decryptAndDecompressCompressedContainer cc
  where
    -- Given the decompressed size and the decompressed 'ByteString', validate
    -- that the length of the 'ByteString' is equal to the decompressed size.
    validateDecompressedSize
      :: Monad m
      => Word32
      -> ByteString
      -> ExceptT DecryptAndDecompressError m ByteString
    validateDecompressedSize expected decompressedBs
      | fromIntegral expected == actual = pure decompressedBs
      | otherwise = left (DecryptAndDecompressInvalidDecompressedDataSizeError expected (fromIntegral actual))
      where
        actual :: Int
        actual = BS.length decompressedBs

    decryptAndDecompressCompressedContainer
      :: CompressedContainer c
      -> ExceptT DecryptAndDecompressError IO ByteString
    decryptAndDecompressCompressedContainer cc = do
      UnencryptedCompressedContainerData
        { uccdUncompressedDataSize
        , uccdData
        } <-
          firstExceptT DecryptAndDecompressDecryptionError
            . hoistEither
            $ decryptCompressedContainer k cc
      decompressed <-
        firstExceptT
          (DecryptAndDecompressDecompressionError . mkError)
          (decompress uccdData)
      validateDecompressedSize uccdUncompressedDataSize decompressed
      where
        mkError :: DecompressionError s -> SomeDecompressionError
        mkError err =
          case err of
            DecompressionErrorBzip2 _ -> SomeDecompressionErrorBzip2 err
            DecompressionErrorGzip _ -> SomeDecompressionErrorGzip err
            DecompressionErrorLzma _ -> SomeDecompressionErrorLzma err
