module RuneScape.Cache.Container.Encryption
  ( MaybeEncrypted (..)
  , toUnencryptedBytes
  , decrypt
  ) where

import qualified Crypto.Cipher.Xtea as Xtea
import Data.ByteArray
  ( ByteArrayAccess (..), convert )
import Data.ByteString
  ( ByteString )
import Prelude hiding
  ( length )

-- | Represents a byte array that /may/ be encrypted.
data MaybeEncrypted a = MaybeEncrypted a
  deriving stock (Show, Eq)

instance ByteArrayAccess a => ByteArrayAccess (MaybeEncrypted a) where
  length (MaybeEncrypted a) = length a
  withByteArray (MaybeEncrypted a) = withByteArray a

-- | Treat this as an unencrypted value and return the raw bytes.
toUnencryptedBytes :: MaybeEncrypted a -> a
toUnencryptedBytes (MaybeEncrypted a) = a

-- | Treat this as an encrypted value and decrypt it.
decrypt :: ByteArrayAccess a => Xtea.SymmetricKey -> MaybeEncrypted a -> Either Xtea.DecryptionError ByteString
decrypt k (MaybeEncrypted ba) = Xtea.decrypt k (convert ba)
