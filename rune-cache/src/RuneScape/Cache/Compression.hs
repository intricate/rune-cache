{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module RuneScape.Cache.Compression
  ( Compression (..)
  , CompressionType (..)
  , CompressedData (..)
  , unCompressedData

  , DecompressionError (..)
  , Bzip2DecompressionError (..)
  , GzipDecompressionError (..)
  , LzmaDecompressionError (..)
  , SomeDecompressionError (..)
  , decompress
  ) where

import Control.Exception
  ( IOException )
import Control.Monad.Trans.Except
  ( ExceptT )
import Control.Monad.Trans.Except.Extra
  ( firstExceptT, newExceptT )
import Data.ByteString
  ( ByteString )
import Data.Conduit
  ( runConduitRes, tryC, yield, (.|) )
import qualified Data.Conduit.BZlib as BZlib
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Lzma as Lzma
import qualified Data.Conduit.Zlib as Zlib
import qualified Data.Streaming.Zlib as Zlib
import Prelude

-- | @bzip2@ header.
--
-- It appears that Jagex doesn't include this header in @bzip2@-compressed
-- data. This is possibly done in order to save 4 bytes. As a result, we need
-- to prepend it before decompressing.
bzip2Header :: ByteString
bzip2Header = "BZh1"

-- | Compression type tag.
data Compression
  = -- | @bzip2@ compression.
    Bzip2
  | -- | @gzip@ compression.
    Gzip
  | -- | LZMA compression.
    Lzma

-- | Compression type.
data CompressionType (c :: Compression) where
  -- | @bzip2@ compression.
  CompressionTypeBzip2 :: CompressionType 'Bzip2

  -- | @gzip@ compression.
  CompressionTypeGzip :: CompressionType 'Gzip

  -- | LZMA compression.
  CompressionTypeLzma :: CompressionType 'Lzma

deriving instance Show (CompressionType c)

deriving instance Eq (CompressionType c)

-- | Compressed data.
data CompressedData (c :: Compression) where
  -- | @bzip2@-compressed data.
  CompressedDataBzip2 :: ByteString -> CompressedData 'Bzip2

  -- | @gzip@-compressed data.
  CompressedDataGzip :: ByteString -> CompressedData 'Gzip

  -- | LZMA-compressed data.
  CompressedDataLzma :: ByteString -> CompressedData 'Lzma

deriving instance Show (CompressedData c)

deriving instance Eq (CompressedData c)

-- | Get the raw compressed 'ByteString' from a 'CompressedData' value.
unCompressedData :: CompressedData c -> ByteString
unCompressedData c =
  case c of
    CompressedDataBzip2 bs -> bs
    CompressedDataGzip bs -> bs
    CompressedDataLzma bs -> bs

-- | @bzip2@ decompression error.
data Bzip2DecompressionError
  = Bzip2DecompressionIoExceptionError !IOException
  deriving stock (Show, Eq)

-- | @gzip@ decompression error.
data GzipDecompressionError
  = GzipDecompressionZlibExceptionError !Zlib.ZlibException
  deriving stock (Show)

-- 'Zlib.ZlibException' doesn't have an 'Eq' instance, so we'll manually
-- define one for 'GzipDecompressionError'.
instance Eq GzipDecompressionError where
  GzipDecompressionZlibExceptionError (Zlib.ZlibException x) == GzipDecompressionZlibExceptionError (Zlib.ZlibException y) =
    x == y

-- | LZMA decompression error.
data LzmaDecompressionError
  = LzmaDecompressionIoExceptionError !IOException
  deriving stock (Show, Eq)

-- | Decompression error.
data DecompressionError (c :: Compression) where
  -- | @bzip2@ decompression error.
  DecompressionErrorBzip2 :: Bzip2DecompressionError -> DecompressionError 'Bzip2

  -- | @gzip@ decompression error.
  DecompressionErrorGzip :: GzipDecompressionError -> DecompressionError 'Gzip

  -- | LZMA decompression error.
  DecompressionErrorLzma :: LzmaDecompressionError -> DecompressionError 'Lzma

deriving instance Show (DecompressionError c)

deriving instance Eq (DecompressionError c)

-- | Decompress data.
decompress :: CompressedData c -> ExceptT (DecompressionError c) IO ByteString
decompress c =
  case c of
    CompressedDataBzip2 bs -> do
      firstExceptT (DecompressionErrorBzip2 . Bzip2DecompressionIoExceptionError)
        . newExceptT
        . runConduitRes
        -- It appears that Jagex doesn't include this header in
        -- @bzip2@-compressed data. This is possibly done in order to save 4
        -- bytes. As a result, we need to prepend it before decompressing.
        $ yield (bzip2Header <> bs)
            .| tryC (BZlib.bunzip2 .| C.foldl (<>) "")
    CompressedDataGzip bs -> do
      firstExceptT (DecompressionErrorGzip . GzipDecompressionZlibExceptionError)
        . newExceptT
        . runConduitRes
        $ yield bs
            .| tryC (Zlib.ungzip .| C.foldl (<>) "")
    CompressedDataLzma bs ->
      firstExceptT (DecompressionErrorLzma . LzmaDecompressionIoExceptionError)
        . newExceptT
        . runConduitRes
        $ yield bs
            .| tryC (Lzma.decompress Nothing .| C.foldl (<>) "")

-- | Some kind of decompression error.
data SomeDecompressionError
  = SomeDecompressionErrorBzip2 !(DecompressionError 'Bzip2)
  | SomeDecompressionErrorGzip !(DecompressionError 'Gzip)
  | SomeDecompressionErrorLzma !(DecompressionError 'Lzma)
  deriving stock (Show, Eq)
