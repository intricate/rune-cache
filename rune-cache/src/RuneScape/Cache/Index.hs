module RuneScape.Cache.Index
  ( IndexEntry (..)
  , indexEntrySize
  , getIndexEntry
  ) where

import Data.Binary.Get
  ( Get )
import Data.Binary.Get.Word24
  ( getWord24be )
import Data.Word
  ( Word32 )
import Data.Word.Word24
  ( Word24 )
import Prelude
import RuneScape.Cache.Block
  ( BlockNumber, getBlockNumber )

-- | Index entry which points to some collection of data in a cache data file.
data IndexEntry = IndexEntry
  { -- | Size (in bytes) of the data which this index points to.
    ieDataSize :: !Word24
  , -- | Starting data block number.
    ieBlockNumber :: !BlockNumber
  } deriving stock (Show, Eq)

-- | Index entry size in bytes.
indexEntrySize :: Word32
indexEntrySize = 6

getIndexEntry :: Get IndexEntry
getIndexEntry = IndexEntry <$> getWord24be <*> getBlockNumber
