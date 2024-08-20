{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Smartly-encoded unsigned integers.
--
-- For information on how this \"smart\" encoding is done, see the
-- documentation for 'SmartWord', 'getSmartWordbe', and 'putSmartWordbe'.
--
-- At the time of writing this, it's not entirely clear to me where the
-- \"smart\" naming convention comes from. I assume that it refers to the
-- \"smart\" binary encoding of the value, but I'm not sure where it
-- originates from, i.e. whether it's from Jagex or some other party.
--
-- Regardless, I've seen this terminology being used in
-- [RuneLite](https://github.com/runelite/runelite),
-- [OpenRS2](https://github.com/openrs2/openrs2), and
-- [Jagex-Store-5](https://github.com/Guthix/Jagex-Store-5), so I've also
-- opted to use it.
module Data.Word.Smart
  ( -- * Smart word
    SmartWord (SmartWord16, SmartWord32)
  , mkSmartWord16
  , mkSmartWord32
  , toWord32

  -- * Binary encoding
  , getSmartWordbe
  , putSmartWordbe
  ) where

import Data.Binary.Get
  ( Get, getWord16be, getWord32be, getWord8, lookAhead )
import Data.Binary.Put
  ( Put, putWord16be, putWord32be )
import Data.Bits
  ( (.&.) )
import Data.Word
  ( Word16, Word32 )
import Prelude

-- | \"Smart\" word.
--
-- A smart word is one which can be \"smartly\" encoded as either a 'Word16'
-- or a 'Word32' in a binary format. This is done by utilizing the most
-- significant bit (MSB) in order to determine how many bytes to read. If the
-- MSB is set, a 'Word32' should be read and the MSB flipped. If the MSB is
-- clear, a 'Word16' should be read.
--
-- Note that using the MSB as an indicator for 4-byte values inherently limits
-- the maximum encodable value to @0x7FFFFFFF@ ('maxSmartWord32').
--
-- Refer to 'getSmartWordbe' for more information.
newtype SmartWord = MkSmartWord Word32
  deriving stock (Show, Eq)

-- | Get the raw 'Word32' value which represents a 'SmartWord' value.
--
-- This function differs from 'encodableAsWord32' in that 'encodableAsWord32'
-- only returns a 'Word32' if the 'SmartWord' must be encoded in 4 bytes.
toWord32 :: SmartWord -> Word32
toWord32 (MkSmartWord w32) = w32

-- | Maximum value that can be smartly encoded in 2 bytes (@0x7FFF@).
--
-- @0x7FFF@ is the largest 2-byte value in which the most significant bit
-- is clear. Therefore, it's the largest value that can be smartly encoded in
-- 2 bytes.
maxSmartWord16 :: Word16
maxSmartWord16 = 0x7FFF

-- | Maximum value that can be encoded in 4 bytes (@0x7FFFFFFF@),
--
-- @0x7FFFFFFF@ is the largest 4-byte value in which the most significant byte
-- is clear. Therefore, it's the largest \"smart word\" value that can be
-- encoded.
maxSmartWord32 :: Word32
maxSmartWord32 = 0x7FFFFFFF

-- | If the given 'SmartWord' can be smartly encoded in 2 bytes (i.e. it is
-- less than or equal to 'maxSmartWord16'), return it as a 'Word16'.
encodableAsWord16 :: SmartWord -> Maybe Word16
encodableAsWord16 (MkSmartWord w32)
  | w32 <= fromIntegral maxSmartWord16 = Just (fromIntegral w32)
  | otherwise = Nothing

-- | If the given 'Word16' can be smartly encoded in 2 bytes (i.e. it is less
-- than or equal to 'maxSmartWord16'), return it as a 'SmartWord'.
mkSmartWord16 :: Word16 -> Maybe SmartWord
mkSmartWord16 w16
  | w16 <= maxSmartWord16 = Just (MkSmartWord $ fromIntegral w16)
  | otherwise = Nothing

-- | If the given 'SmartWord' must be encoded in 4 bytes (i.e.
-- @'maxSmartWord16' < x <= 'maxSmartWord32'@), return it as a 'Word32'.
encodableAsWord32 :: SmartWord -> Maybe Word32
encodableAsWord32 (MkSmartWord w32)
  | w32 > fromIntegral maxSmartWord16 && w32 <= maxSmartWord32 = Just w32
  | otherwise = Nothing

-- | If the given 'Word32' must be encoded in 4 bytes (i.e.
-- @'maxSmartWord16' < x <= 'maxSmartWord32'@), return it as a 'SmartWord'.
mkSmartWord32 :: Word32 -> Maybe SmartWord
mkSmartWord32 w32
  | w32 > fromIntegral maxSmartWord16 && w32 <= maxSmartWord32 = Just (MkSmartWord w32)
  | otherwise = Nothing

-- | Word that can be smartly encoded in 2 bytes.
pattern SmartWord16 :: Word16 -> SmartWord
pattern SmartWord16 w16 <- (encodableAsWord16 -> Just w16)

-- | Word that must be encoded in 4 bytes.
pattern SmartWord32 :: Word32 -> SmartWord
pattern SmartWord32 w32 <- (encodableAsWord32 -> Just w32)

{-# COMPLETE SmartWord16, SmartWord32 #-}

-- | Read a smartly-encoded word in big-endian format.
--
-- If the most significant bit (MSB) is set, then the result is a
-- 'SmartWord32' (with the MSB flipped). Otherwise, the result is a
-- 'SmartWord16'.
getSmartWordbe :: Get SmartWord
getSmartWordbe = do
  peekedW8 <- lookAhead getWord8
  if peekedW8 .&. 0x80 == 0
    then do
      -- The most significant bit is clear, so read a 'Word16'.
      w16 <- getWord16be
      case mkSmartWord16 w16 of
        Just x -> pure x
        Nothing -> fail "Attempted to decode an invalid SmartWord16"
    else do
      -- The most significant bit is set, so read a 'Word32' and flip the MSB.
      w32 <- (.&. 0x7FFFFFFF) <$> getWord32be
      case mkSmartWord32 w32 of
        Just x -> pure x
        Nothing -> fail "Attempted to decode an invalid SmartWord32"

-- | Smartly encode a word in big-endian format.
--
-- * If a 'SmartWord16' is being encoded, encode it in 2 bytes.
-- * If a 'SmartWord32' is being encoded, set the most significant bit and
-- encode it in 4 bytes.
putSmartWordbe :: SmartWord -> Put
putSmartWordbe (SmartWord16 w16) = putWord16be w16
putSmartWordbe (SmartWord32 w32) =
  -- Set the most significant bit in order to indicate that this is a 4 byte
  -- value.
  putWord32be (w32 .&. 0x7FFFFFFF)
