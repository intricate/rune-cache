{-# LANGUAGE ScopedTypeVariables #-}

module Data.Binary.Get.Delta
  ( getDeltaEncodedValues
  ) where

import Control.Monad
  ( foldM )
import Data.Binary.Get
  ( Get )
import Data.Word
  ( Word32 )
import Prelude

-- | Decode a collection of delta-encoded values.
getDeltaEncodedValues :: forall a. Num a => Word32 -> Get a -> Get [a]
getDeltaEncodedValues n getDelta = reverse <$> foldM f [] [0 .. n - 1]
  where
    f :: [a] -> Word32 -> Get [a]
    f [] _ = pure <$> getDelta
    f vals@(lastVal : _) _ = do
      delta <- getDelta
      pure (delta + lastVal : vals)
