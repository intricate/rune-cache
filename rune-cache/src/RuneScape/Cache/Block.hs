module RuneScape.Cache.Block
  ( -- * Block number
    BlockNumber (..)
  , getBlockNumber

    -- * Block header
  , BlockHeaderType (..)
  , fromGroupId
    -- ** Small header
  , SmallBlockHeader (..)
  , smallBlockHeaderSize
  , getSmallBlockHeader
    -- ** Extended header
  , ExtendedBlockHeader (..)
  , extendedBlockHeaderSize
  , getExtendedBlockHeader

    -- * Block
  , Block (..)
    -- ** Helpful accessors
  , groupId
  , relativeBlockNumber
  , nextBlockNumber
  , archiveId
  , blockData
    -- ** Byte sizes
  , blockDataSize
  , blockSize
    -- ** Deserialization
  , getBlock
  ) where

import Data.Binary.Get
  ( Get, getByteString, getWord16be )
import Data.Binary.Get.Word24
  ( getWord24be )
import Data.ByteString
  ( ByteString )
import Data.Word
  ( Word16, Word32 )
import Data.Word.Word24
  ( Word24 )
import Prelude
import RuneScape.Cache.Archive
  ( ArchiveId, getArchiveId )
import RuneScape.Cache.Group.Id
  ( ExtendedGroupId
  , GroupId (..)
  , SmallGroupId
  , getExtendedGroupId
  , getSmallGroupId
  )

------------------------------------------------------------------------------
-- Block number
------------------------------------------------------------------------------

-- | Index of a block in a cache data file.
newtype BlockNumber = BlockNumber
  { unBlockNumber :: Word24 }
  deriving stock (Show, Eq)

getBlockNumber :: Get BlockNumber
getBlockNumber = BlockNumber <$> getWord24be

------------------------------------------------------------------------------
-- Block header
------------------------------------------------------------------------------

-- | Type of block header.
data BlockHeaderType
  = -- | Small block header ('SmallBlockHeader').
    BlockHeaderTypeSmall
  | -- | Extended block header ('ExtendedBlockHeader').
    BlockHeaderTypeExtended
  deriving stock (Show, Eq)

-- | Given a 'GroupId', return what type of headers its block's will have.
--
-- If the 'GroupId' is a 'SmallGroupId', then the header type will be
-- 'BlockHeaderTypeSmall'. Otherwise, it will be 'BlockHeaderTypeExtended'.
fromGroupId :: GroupId -> BlockHeaderType
fromGroupId g =
  case g of
    GroupIdSmall _ -> BlockHeaderTypeSmall
    GroupIdExtended _ -> BlockHeaderTypeExtended

-- | Small block header.
data SmallBlockHeader = SmallBlockHeader
  { -- | Identifier of the group associated with this block of data.
    sbhGroupId :: !SmallGroupId
  , -- | Relative identifier of this block within the group.
    --
    -- That is, for the first block within the group, this value will be @0@.
    -- The next will be @1@, then @2@, and so on.
    sbhRelativeBlockNumber :: !Word16
  , -- | Identifier of the next block of data for this group.
    sbhNextBlockNumber :: !BlockNumber
  , -- | Identifier of the archive to which this block belongs.
    sbhArchiveId :: !ArchiveId
  } deriving stock (Show, Eq)

-- | Small block header size in bytes.
smallBlockHeaderSize :: Word32
smallBlockHeaderSize = 8

getSmallBlockHeader :: Get SmallBlockHeader
getSmallBlockHeader =
  SmallBlockHeader
    <$> getSmallGroupId
    <*> getWord16be
    <*> getBlockNumber
    <*> getArchiveId

-- | Extended block header.
data ExtendedBlockHeader = ExtendedBlockHeader
  { -- | Identifier of the group associated with this block of data.
    ebhGroupId :: !ExtendedGroupId
  , -- | Relative identifier of this block within the group.
    --
    -- That is, for the first block within the group, this value will be @0@.
    -- The next will be @1@, then @2@, and so on.
    ebhRelativeBlockNumber :: !Word16
  , -- | Identifier of the next block of data for this group.
    ebhNextBlockNumber :: !BlockNumber
  , -- | Identifier of the archive to which this block belongs.
    ebhArchiveId :: !ArchiveId
  } deriving stock (Show, Eq)

-- | Extended block header size in bytes.
extendedBlockHeaderSize :: Word32
extendedBlockHeaderSize = 10

getExtendedBlockHeader :: Get ExtendedBlockHeader
getExtendedBlockHeader =
  ExtendedBlockHeader
    <$> getExtendedGroupId
    <*> getWord16be
    <*> getBlockNumber
    <*> getArchiveId

------------------------------------------------------------------------------
-- Block
------------------------------------------------------------------------------

-- | Data block.
data Block
  = -- | Data block with a small header.
    BlockWithSmallHeader !SmallBlockHeader !ByteString
  | -- | Data block with an extended header.
    BlockWithExtendedHeader !ExtendedBlockHeader !ByteString

-- | Get the group ID from a 'Block'.
groupId :: Block -> GroupId
groupId b =
  case b of
    BlockWithSmallHeader h _ -> GroupIdSmall (sbhGroupId h)
    BlockWithExtendedHeader h _ -> GroupIdExtended (ebhGroupId h)

-- | Get the relative identifier of this block within its group.
--
-- That is, for the first block within a group, this value will be @0@. The
-- next will be @1@, then @2@, and so on.
relativeBlockNumber :: Block -> Word16
relativeBlockNumber b =
  case b of
    BlockWithSmallHeader h _ -> sbhRelativeBlockNumber h
    BlockWithExtendedHeader h _ -> ebhRelativeBlockNumber h

-- | Get the next block number from a 'Block'.
nextBlockNumber :: Block -> BlockNumber
nextBlockNumber b =
  case b of
    BlockWithSmallHeader h _ -> sbhNextBlockNumber h
    BlockWithExtendedHeader h _ -> ebhNextBlockNumber h

-- | Get the archive ID from a 'Block'.
archiveId :: Block -> ArchiveId
archiveId b =
  case b of
    BlockWithSmallHeader h _ -> sbhArchiveId h
    BlockWithExtendedHeader h _ -> ebhArchiveId h

-- | Get a 'Block'\'s payload.
blockData :: Block -> ByteString
blockData b =
  case b of
    BlockWithSmallHeader _ bs -> bs
    BlockWithExtendedHeader _ bs -> bs

-- | Size of a 'Block' (@520@ bytes).
--
-- Regardless of the block header's size, a block is always @520@ bytes. The
-- block's payload size, however, depends on the size of the header (see
-- 'blockDataSize').
blockSize :: Word32
blockSize = 520

-- | Size (in bytes) of a 'Block'\'s payload.
blockDataSize :: BlockHeaderType -> Word32
blockDataSize ht =
  case ht of
    BlockHeaderTypeSmall -> blockSize - smallBlockHeaderSize
    BlockHeaderTypeExtended -> blockSize - extendedBlockHeaderSize

getBlock :: BlockHeaderType -> Get Block
getBlock ht =
  case ht of
    BlockHeaderTypeSmall ->
      BlockWithSmallHeader
        <$> getSmallBlockHeader
        <*> getByteString (fromIntegral $ blockDataSize ht)
    BlockHeaderTypeExtended ->
      BlockWithExtendedHeader
        <$> getExtendedBlockHeader
        <*> getByteString (fromIntegral $ blockDataSize ht)
