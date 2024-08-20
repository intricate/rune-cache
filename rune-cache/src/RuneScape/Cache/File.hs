module RuneScape.Cache.File
  ( FileId (..)
  , getFileId

  , File (..)
  ) where

import Data.Binary.Get
  ( Get, getWord32be )
import Data.ByteString
  ( ByteString )
import Data.Word
  ( Word32 )
import Prelude

------------------------------------------------------------------------------
-- File identifier
------------------------------------------------------------------------------

-- | File identifier.
newtype FileId = FileId
  { unFileId :: Word32 }
  deriving stock (Show, Eq)
  deriving newtype (Ord)

getFileId :: Get FileId
getFileId = FileId <$> getWord32be

------------------------------------------------------------------------------
-- File
------------------------------------------------------------------------------

-- | File.
data File = File
  { -- | File identifier.
    fId :: !FileId
  , -- | Raw file data.
    fData :: !ByteString
  } deriving stock (Show, Eq)
