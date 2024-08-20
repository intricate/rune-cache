module RuneScape.Cache.File.Settings
  ( FileSettings (..)
  ) where

import Data.Word
  ( Word32 )
import Prelude
import RuneScape.Cache.File
  ( FileId )

-- | File settings.
--
-- This contains metadata about a 'File'.
data FileSettings = FileSettings
  { fsId :: !FileId
  , fsNameHash :: !(Maybe Word32)
  } deriving stock (Show, Eq)
