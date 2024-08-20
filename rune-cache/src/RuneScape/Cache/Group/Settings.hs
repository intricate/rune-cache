module RuneScape.Cache.Group.Settings
  ( Version (..)
  , getVersion
  , CompressedSize (..)
  , getCompressedSize
  , UncompressedSize (..)
  , getUncompressedSize
  , GroupSettings (..)
  ) where

import Crypto.Hash
  ( Digest, Whirlpool )
import Data.Binary.Get
  ( Get, getWord32be )
import Data.Map.Strict
  ( Map )
import Data.Word
  ( Word32 )
import Prelude
import RuneScape.Cache.File
  ( FileId )
import RuneScape.Cache.File.Settings
  ( FileSettings )
import RuneScape.Cache.Group.Id
  ( GroupId )

-- | Group settings version.
newtype Version = Version
  { unVersion :: Word32 }
  deriving stock (Show, Eq)

getVersion :: Get Version
getVersion = Version <$> getWord32be

-- | Compressed size (in bytes) of a group.
newtype CompressedSize = CompressedSize Word32
  deriving stock (Show, Eq)
  deriving newtype (Num)

getCompressedSize :: Get CompressedSize
getCompressedSize = CompressedSize <$> getWord32be

-- | Uncompressed size (in bytes) of a group.
newtype UncompressedSize = UncompressedSize Word32
  deriving stock (Show, Eq)
  deriving newtype (Num)

getUncompressedSize :: Get UncompressedSize
getUncompressedSize = UncompressedSize <$> getWord32be

-- | Group settings.
--
-- This contains metadata about a 'Group' and the 'File's contained within it.
data GroupSettings = GroupSettings
  { gsId :: !GroupId
  , gsNameHash :: !(Maybe Word32)
  , gsCompressedChecksum :: !Word32
  , gsUncompressedChecksum :: !(Maybe Word32)
  , gsWhirlpoolDigest :: !(Maybe (Digest Whirlpool))
  , gsSizes :: !(Maybe (CompressedSize, UncompressedSize))
  , gsVersion :: !Version
  , gsFileSettings :: !(Map FileId FileSettings)
  } deriving stock (Show, Eq)
