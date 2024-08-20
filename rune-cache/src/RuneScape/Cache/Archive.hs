module RuneScape.Cache.Archive
  ( ArchiveId (..)
  , getArchiveId
  , settingsArchiveId
  ) where

import Data.Binary.Get
  ( Get, getWord8 )
import Data.Word
  ( Word8 )
import Prelude

------------------------------------------------------------------------------
-- Archive identifier
------------------------------------------------------------------------------

-- | Archive identifier.
newtype ArchiveId = ArchiveId
  { unArchiveId :: Word8 }
  deriving stock (Show, Eq)
  deriving newtype (Ord, Enum, Num, Real, Integral)

-- Technically, we could derive a 'Bounded' instance from the 'Word8' and that
-- would be correct. However, it doesn't hurt to be explicit about the bounds
-- of an 'ArchiveId'
instance Bounded ArchiveId where
  -- Minimum archive identifier (@0@).
  minBound = ArchiveId 0

  -- Maximum archive identifier (@255@).
  maxBound = ArchiveId 255

getArchiveId :: Get ArchiveId
getArchiveId = ArchiveId <$> getWord8

-- | 'ArchiveId' of the \"archive settings\" archive.
--
-- This is a special archive with the ID @255@ and it contains important
-- metadata about all available archives in the cache. For more information,
-- see 'ArchiveSettings'.
settingsArchiveId :: ArchiveId
settingsArchiveId = maxBound
