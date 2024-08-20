module RuneScape.Cache.Group.Id
  ( SmallGroupId (..)
  , getSmallGroupId
  , ExtendedGroupId (unExtendedGroupId)
  , mkExtendedGroupId
  , getExtendedGroupId
  , GroupId (..)
  , toWord16
  , fromWord16
  , toWord32
  , fromWord32
  ) where

import Data.Binary.Get
  ( Get, getWord16be, getWord32be )
import Data.Word
  ( Word16, Word32 )
import Prelude

-- | Small group identifier (2 bytes).
newtype SmallGroupId = SmallGroupId
  { unSmallGroupId :: Word16 }
  deriving stock (Show, Eq)
  deriving newtype (Ord)

getSmallGroupId :: Get SmallGroupId
getSmallGroupId = SmallGroupId <$> getWord16be

-- | Extended group identifier (4 bytes).
--
-- Note that an extended group identifier /must/ be greater than @0xFFFF@. For
-- smaller group identifiers, see 'SmallGroupId'.
newtype ExtendedGroupId = ExtendedGroupId
  { unExtendedGroupId :: Word32 }
  deriving stock (Show, Eq)
  deriving newtype (Ord)

-- | Construct an extended group identifier.
--
-- If the provided 'Word32' is less than or equal to @0xFFFF@, the result is
-- 'Nothing'.
mkExtendedGroupId :: Word32 -> Maybe ExtendedGroupId
mkExtendedGroupId w32
  | w32 > 0xFFFF = Just (ExtendedGroupId w32)
  | otherwise = Nothing

getExtendedGroupId :: Get ExtendedGroupId
getExtendedGroupId = ExtendedGroupId <$> getWord32be

-- | Either a small (2-byte) or extended (4-byte) group identifier.
data GroupId
  = -- | 2-byte group identifier.
    GroupIdSmall !SmallGroupId
  | -- | 4-byte group identifier.
    GroupIdExtended !ExtendedGroupId
  deriving stock (Show, Eq)

instance Ord GroupId where
  GroupIdSmall (SmallGroupId x) `compare` y =
    case y of
      GroupIdSmall (SmallGroupId z) -> x `compare` z
      GroupIdExtended (ExtendedGroupId z) -> fromIntegral x `compare` z
  GroupIdExtended (ExtendedGroupId x) `compare` y =
    case y of
      GroupIdSmall (SmallGroupId z) -> x `compare` fromIntegral z
      GroupIdExtended (ExtendedGroupId z) -> x `compare` z

-- | Convert a 'GroupId' to a 'Word16'.
toWord16 :: GroupId -> Maybe Word16
toWord16 (GroupIdSmall (SmallGroupId w16)) = Just w16
toWord16 _ = Nothing

-- | Convert a 'Word16' to a 'GroupId'.
fromWord16 :: Word16 -> GroupId
fromWord16 w16 = GroupIdSmall (SmallGroupId w16)

-- | Convert a 'GroupId' to a 'Word32'.
toWord32 :: GroupId -> Word32
toWord32 (GroupIdSmall (SmallGroupId w16)) = fromIntegral w16
toWord32 (GroupIdExtended (ExtendedGroupId w32)) = w32

-- | Convert a 'Word32' to a 'GroupId'.
fromWord32 :: Word32 -> GroupId
fromWord32 w32
  | w32 <= 0xFFFF = GroupIdSmall (SmallGroupId $ fromIntegral w32)
  | otherwise = GroupIdExtended (ExtendedGroupId w32)
