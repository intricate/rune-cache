{-# LANGUAGE BangPatterns #-}

-- | Implementation of the XTEA (eXtended Tiny Encryption Algorithm) block
-- cipher.
--
-- Its specification can be found
-- [here](https://www.cix.co.uk/~klockstone/xtea.pdf).
module Crypto.Cipher.Xtea
  ( SymmetricKey (..)

  , Endianness (..)

  , EncryptionError (..)
  , encryptBlock
  , encrypt

  , DecryptionError (..)
  , decryptBlock
  , decrypt
  ) where

import Control.Monad
  ( replicateM )
import Data.Binary.Get
  ( Get, getWord32be, getWord32le, runGetOrFail )
import Data.Binary.Put
  ( Put, putWord32be, putWord32le, runPut )
import Data.Bits
  ( shiftL, shiftR, xor, (.&.) )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word
  ( Word32 )
import Prelude hiding
  ( sum )

-- | 128-bit XTEA symmetric key.
data SymmetricKey = SymmetricKey {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32

-- | Get a specific 32-bit block of a 'SymmetricKey'.
unsafeGetSymmetricKeyBlock :: SymmetricKey -> Word32 -> Word32
unsafeGetSymmetricKeyBlock (SymmetricKey k0 k1 k2 k3) i =
  case i of
    0 -> k0
    1 -> k1
    2 -> k2
    3 -> k3
    _ -> error $ "impossible: requested index " <> show i <> " is out of range"

-- | XTEA's block size in bytes.
xteaBlockSize :: Int
xteaBlockSize = 8

delta :: Word32
delta = 0x9E3779B9

rounds :: Word32
rounds = 32

-- | Byte ordering.
data Endianness
  = -- | Little-endian byte ordering.
    LittleEndian
  | -- | Big-endian byte ordering.
    BigEndian
  deriving stock (Show, Eq)

-- | Interpret a 'ByteString' as a list of 'Word32' tuples.
--
-- If the length of the 'ByteString' is not a multiple of 64 bits (8 bytes),
-- the result is 'Nothing'.
byteStringToXteaBlocks :: Endianness -> ByteString -> Maybe [(Word32, Word32)]
byteStringToXteaBlocks endianness bs
  | remainder == 0 =
      case runGetOrFail getBlocks (LBS.fromStrict bs) of
        Left _ -> Nothing
        Right (_, _, blocks) -> Just blocks
  | otherwise = Nothing
  where
    numXteaBlocks :: Int
    remainder :: Int
    (numXteaBlocks, remainder) = BS.length bs `divMod` xteaBlockSize

    getWord32 :: Get Word32
    getWord32 =
      case endianness of
        LittleEndian -> getWord32le
        BigEndian -> getWord32be

    getBlocks :: Get [(Word32, Word32)]
    getBlocks = replicateM numXteaBlocks ((,) <$> getWord32 <*> getWord32)

-- | Interpret a list of 'Word32' tuples as a 'ByteString'.
xteaBlocksToByteString :: Endianness -> [(Word32, Word32)] -> ByteString
xteaBlocksToByteString endianness bs = LBS.toStrict (runPut putBlocks)
  where
    putWord32 :: Word32 -> Put
    putWord32 =
      case endianness of
        LittleEndian -> putWord32le
        BigEndian -> putWord32be

    putBlocks :: Put
    putBlocks = mapM_ (\(b0, b1) -> putWord32 b0 >> putWord32 b1) bs

-- | XTEA encrypt a 64-bit block.
--
-- This function is based on the following C implementation:
--
-- @
--   void encipher(unsigned int num_rounds, uint32_t v[2], uint32_t const key[4]) {
--       unsigned int i;
--       uint32_t v0=v[0], v1=v[1], sum=0, delta=0x9E3779B9;
--       for (i=0; i < num_rounds; i++) {
--           v0 += (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
--           sum += delta;
--           v1 += (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum>>11) & 3]);
--       }
--       v[0]=v0; v[1]=v1;
--   }
-- @
encryptBlock :: SymmetricKey -> (Word32, Word32) -> (Word32, Word32)
encryptBlock k (startingV0, startingV1) = go rounds 0 startingV0 startingV1
  where
    go
      :: Word32
      -- ^ Number of rounds
      -> Word32
      -- ^ Sum
      -> Word32
      -- ^ v0
      -> Word32
      -- ^ v1
      -> (Word32, Word32)
    go 0 _ !v0 !v1 = (v0, v1)
    go !n !sum !v0 !v1 = go (n - 1) nextSum nextV0 nextV1
      where
        -- v0 += (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
        nextV0 :: Word32
        nextV0 =
          v0
            + ( (((v1 `shiftL` 4) `xor` (v1 `shiftR` 5)) + v1)
                  `xor` (sum + unsafeGetSymmetricKeyBlock k (sum .&. 3))
              )

        -- sum += delta;
        nextSum :: Word32
        nextSum = sum + delta

        -- v1 += (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum>>11) & 3]);
        nextV1 :: Word32
        nextV1 =
          v1
            + ( (((nextV0 `shiftL` 4) `xor` (nextV0 `shiftR` 5)) + nextV0)
                  `xor` (nextSum + unsafeGetSymmetricKeyBlock k ((nextSum `shiftR` 11) .&. 3))
              )

-- | XTEA encryption error.
data EncryptionError
  = -- | Input length is not a multiple of XTEA's block size (64 bits).
    EncryptionInvalidInputLengthError !Int
  deriving stock (Show, Eq)

encryptWith :: Endianness -> SymmetricKey -> ByteString -> Either EncryptionError ByteString
encryptWith endianness k bs = do
  blocks <-
    case byteStringToXteaBlocks endianness bs of
      Nothing -> Left $ EncryptionInvalidInputLengthError (BS.length bs)
      Just x -> Right x
  let encryptedBlocks = map (encryptBlock k) blocks
  Right (xteaBlocksToByteString endianness encryptedBlocks)

-- | XTEA encrypt a 'ByteString'.
--
-- Endianness defaults to 'BigEndian'.
encrypt :: SymmetricKey -> ByteString -> Either EncryptionError ByteString
encrypt = encryptWith BigEndian

-- | Decrypt an XTEA-encrypted 64-bit block.
--
-- This function is based on the following C implementation:
--
-- @
--   void decipher(unsigned int num_rounds, uint32_t v[2], uint32_t const key[4]) {
--       unsigned int i;
--       uint32_t v0=v[0], v1=v[1], delta=0x9E3779B9, sum=delta*num_rounds;
--       for (i=0; i < num_rounds; i++) {
--           v1 -= (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum>>11) & 3]);
--           sum -= delta;
--           v0 -= (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
--       }
--       v[0]=v0; v[1]=v1;
--   }
-- @
decryptBlock :: SymmetricKey -> (Word32, Word32) -> (Word32, Word32)
decryptBlock k (startingV0, startingV1) = go rounds (delta * rounds) startingV0 startingV1
  where
    go
      :: Word32
      -- ^ Number of rounds
      -> Word32
      -- ^ Sum
      -> Word32
      -- ^ v0
      -> Word32
      -- ^ v1
      -> (Word32, Word32)
    go 0 _ !v0 !v1 = (v0, v1)
    go !n !sum !v0 !v1 = go (n - 1) nextSum nextV0 nextV1
      where
        -- v1 -= (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum>>11) & 3]);
        nextV1 :: Word32
        nextV1 =
          v1
            - ( (((v0 `shiftL` 4) `xor` (v0 `shiftR` 5)) + v0)
                  `xor` (sum + unsafeGetSymmetricKeyBlock k ((sum `shiftR` 11) .&. 3))
              )

        -- sum -= delta;
        nextSum :: Word32
        nextSum = sum - delta

        -- v0 -= (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
        nextV0 :: Word32
        nextV0 =
          v0
            - ( (((nextV1 `shiftL` 4) `xor` (nextV1 `shiftR` 5)) + nextV1)
                  `xor` (nextSum + unsafeGetSymmetricKeyBlock k (nextSum .&. 3))
              )

-- | XTEA decryption error.
data DecryptionError
  = -- | Input length is not a multiple of XTEA's block size (64 bits).
    DecryptionInvalidInputLengthError !Int
  deriving stock (Show, Eq)

decryptWith :: Endianness -> SymmetricKey -> ByteString -> Either DecryptionError ByteString
decryptWith endianness k bs = do
  blocks <-
    case byteStringToXteaBlocks endianness bs of
      Nothing -> Left $ DecryptionInvalidInputLengthError (BS.length bs)
      Just x -> Right x
  let decryptedBlocks = map (decryptBlock k) blocks
  Right (xteaBlocksToByteString endianness decryptedBlocks)

-- | Decrypt an XTEA-encrypted 'ByteString'.
--
-- Endianness defaults to 'BigEndian'.
decrypt :: SymmetricKey -> ByteString -> Either DecryptionError ByteString
decrypt = decryptWith BigEndian
