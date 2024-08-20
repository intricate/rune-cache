module Data.Binary.Get.Word24
  ( getWord24be
  , getWord24le
  ) where

import Data.Binary.Get
  ( Get, getWord8 )
import Data.Bits
  ( shiftL, (.|.) )
import Data.Word
  ( Word8 )
import Data.Word.Word24
  ( Word24 )
import Prelude

get3Bytes :: Get (Word8, Word8, Word8)
get3Bytes = (,,) <$> getWord8 <*> getWord8 <*> getWord8
{-# INLINE get3Bytes #-}

-- | Read a 'Word24' in big-endian format.
getWord24be :: Get Word24
getWord24be = do
  (b0, b1, b2) <- get3Bytes
  pure $
    (fromIntegral b0 `shiftL` 16)
      .|. ( fromIntegral b1 `shiftL` 8)
      .|. fromIntegral b2
{-# INLINE getWord24be #-}

-- | Read a 'Word24' in little-endian format.
getWord24le :: Get Word24
getWord24le = do
  (b0, b1, b2) <- get3Bytes
  pure $
    (fromIntegral b2 `shiftL` 16)
      .|. ( fromIntegral b1 `shiftL` 8)
      .|. fromIntegral b0
{-# INLINE getWord24le #-}
