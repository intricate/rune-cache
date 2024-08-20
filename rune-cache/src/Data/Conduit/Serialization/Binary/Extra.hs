module Data.Conduit.Serialization.Binary.Extra
  ( conduitGetEither
  ) where

import Data.Binary.Get
  ( ByteOffset, Decoder (..), Get, pushChunk, runGetIncremental )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as BS
import Data.Conduit
  ( ConduitT, await, yield )
import Prelude

-- | Runs getter repeatedly on a input stream, but we return an 'Either' type
-- instead of raising an exception.
conduitGetEither
  :: Monad m
  => Get b
  -> ConduitT ByteString (Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, b)) m ()
conduitGetEither g = start
  where
    start = do
      mx <- await
      case mx of
        Nothing -> pure ()
        Just x -> go (runGetIncremental g `pushChunk` x)
    go (Done bs o v) = do
      yield $! Right (bs, o, v)
      if BS.null bs
        then start
        else go (runGetIncremental g `pushChunk` bs)
    go (Fail u o e) = yield $! Left (u, o, e)
    go (Partial n) = await >>= (go . n)
