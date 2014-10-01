{-# LANGUAGE DeriveDataTypeable #-}

module Data.Traversable.Cached where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (liftIO)
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Data
import Data.Traversable
import System.Directory (copyFile, doesFileExist, removeFile, renameFile)
import System.FilePath ((<.>))

newtype Cached t a = Cached (t a)
  deriving (Data, Eq, Ord, Read, Show, Typeable)

-- | The @Functor@ instance of @(Cached t)@ is as lazy as the type @t@.
instance Functor t => Functor (Cached t) where
    fmap = \f (Cached as) -> Cached (fmap f as)
    {-# INLINE fmap #-}

-- | Traverse the given structure, retrieving values from and storing values in
-- the cache at the given path. Only the element type need be serializable. The
-- struture must be lazy in the values for this to be effective.
cached :: (Binary a, Traversable t) => FilePath -> t a -> IO (t a)
cached path dat = do
    cacheExists <- doesFileExist path
    cache <- if cacheExists then LBS.readFile path else return LBS.empty
    newExists <- doesFileExist newPath
    when newExists $ removeFile newPath
    as <- State.evalStateT (traverse cached_go dat) cache
    removeFile newPath
    return as
  where
    newPath = path <.> "new"

    appendToCache :: Binary a => a -> IO ()
    appendToCache a = do
        BS.appendFile newPath $! LBS.toStrict $! encode a
        renameFile newPath path
        copyFile path newPath

    cached_go :: Binary a => a -> StateT LBS.ByteString IO a
    cached_go computed = do
        decoded <- decodeOrFail <$> State.get
        case decoded of
            Left _ -> do
                State.put LBS.empty
                seq computed $ liftIO $ appendToCache computed
                return computed
            Right (nextCache, _, retrieved) -> do
                State.put nextCache
                liftIO $ appendToCache retrieved
                return retrieved
