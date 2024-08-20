-- | Implementation of the (non-cryptographic) hash function, @djb2@.
--
-- This is based on Daniel J. Bernstein's algorithm which was first reported
-- in @comp.lang.c@. The C implementation can be found
-- [here](http://www.cse.yorku.ca/~oz/hash.html#djb2).
module Data.Hash.Djb2
  ( hash
  ) where

import Data.Bits
  ( shiftL )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as BS
import Data.Foldable
  ( foldl' )
import Data.Word
  ( Word32, Word8 )
import Prelude

-- | Hash a strict 'ByteString' using the @djb2@ algorithm.
--
-- This function is based on the following C implementation:
--
-- @
--   unsigned long
--   hash(unsigned char *str)
--   {
--       unsigned long hash = 5381;
--       int c;
--
--       while (c = *str++)
--           hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
--
--       return hash;
--   }
-- @
hash :: ByteString -> Word32
hash = foldl' f 5381 . BS.unpack
  where
    f :: Word32 -> Word8 -> Word32
    f accHash c =
      -- Since 'c' is a 'Word8', it's safe to convert it to a 'Word32'
      let cW32 :: Word32
          cW32 = fromIntegral c
      in ((accHash `shiftL` 5) + accHash) + cW32
