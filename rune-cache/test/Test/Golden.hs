module Test.Golden
  ( goldenTestByteString
  ) where

import Control.Monad.IO.Class
  ( liftIO )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as BS
import GHC.Stack
  ( HasCallStack, withFrozenCallStack )
import Hedgehog
  ( PropertyT, (===) )
import Prelude

goldenTestByteString
  :: HasCallStack
  => ByteString
  -> FilePath
  -> PropertyT IO ()
goldenTestByteString x path = withFrozenCallStack $ do
  bs <- liftIO (BS.readFile path)
  x === bs
