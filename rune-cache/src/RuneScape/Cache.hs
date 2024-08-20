-- | High-level API for RuneScape cache operations.
module RuneScape.Cache
  ( -- * Cache
    Cache

    -- ** Construction
  , CacheOperationError (..)
  , withCache

    -- ** Unsafe construction
  , unsafeAllocateCache
  , releaseCache

    -- * Read
  , readGroup
  , readFile
  ) where

import Control.Monad.Trans.Except
  ( ExceptT )
import Control.Monad.Trans.Except.Extra
  ( bimapExceptT, bracketExceptionT, catchExceptT, firstExceptT, right )
import qualified Crypto.Cipher.Xtea as Xtea
import Path
  ( PathException, parseSomeDir )
import Prelude hiding
  ( readFile )
import RuneScape.Cache.Archive
  ( ArchiveId )
import RuneScape.Cache.DiskStore
  ( AllocateDiskStoreError
  , DiskStore
  , ReadFileError
  , ReadGroupError
  , releaseDiskStore
  , unsafeAllocateDiskStore
  )
import qualified RuneScape.Cache.DiskStore as DiskStore
import RuneScape.Cache.File
  ( File, FileId )
import RuneScape.Cache.Group
  ( Group )
import RuneScape.Cache.Group.Id
  ( GroupId )

-- TODO: Cache archive settings and deserialized groups.

-- | RuneScape cache.
newtype Cache = Cache
  { cStore :: DiskStore
  }

-- | Error allocating resources for a 'Cache'.
data AllocateCacheError
  = -- | Provided RuneScape cache directory is invalid.
    AllocateCacheInvalidCacheDirectory !PathException
  | -- | Error allocating a 'DiskStore'.
    AllocateCacheDiskStoreError !AllocateDiskStoreError
  deriving (Show, Eq)

-- | Allocate resources for and construct a 'Cache'.
--
-- Note that it is unsafe to use this function without ensuring that
-- 'releaseCache' is appropriately called. For a safer alternative, consider
-- using 'withCache' instead.
unsafeAllocateCache :: FilePath -> ExceptT AllocateCacheError IO Cache
unsafeAllocateCache rawPath = do
  path <-
    parseSomeDir rawPath
      `catchExceptT` AllocateCacheInvalidCacheDirectory
  bimapExceptT AllocateCacheDiskStoreError Cache (unsafeAllocateDiskStore path)

-- | Release the resources associated with a 'Cache'.
releaseCache :: Cache -> IO ()
releaseCache = releaseDiskStore . cStore

-- | Error running a computation on the 'Cache' using 'withCache'.
data CacheOperationError err
  = -- | Error allocating resources for the 'Cache'.
    CacheOperationAllocationError !AllocateCacheError
  | -- | Custom error that can occur as a result of a computation on the
    -- 'Cache'.
    CacheOperationError !err
  deriving stock (Show, Eq)

-- | Open the 'Cache' and pass it to a provided computation. The resources
-- associated with the 'Cache' will be released when the computation returns,
-- whether by normal termination or by raising an exception.
withCache
  :: FilePath
  -- ^ Path to the root directory of the RuneScape cache.
  -> (Cache -> ExceptT err IO a)
  -- ^ Computation to run on the 'Cache'.
  -> ExceptT (CacheOperationError err) IO a
withCache path f =
  bracketExceptionT
    (firstExceptT CacheOperationAllocationError $ unsafeAllocateCache path)
    (right . releaseCache)
    (firstExceptT CacheOperationError . f)

-- | Read a 'Group' from the cache.
readGroup :: Cache -> ArchiveId -> GroupId -> Maybe Xtea.SymmetricKey -> ExceptT ReadGroupError IO Group
readGroup c = DiskStore.readGroup cStore
  where
    Cache
      { cStore
      } = c

-- | Read a 'File' from the cache.
readFile :: Cache -> ArchiveId -> GroupId -> FileId -> Maybe Xtea.SymmetricKey -> ExceptT ReadFileError IO File
readFile c = DiskStore.readFile cStore
  where
    Cache
      { cStore
      } = c
