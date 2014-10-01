module Data.Traversable.Cached where

import Control.Applicative
import Control.Arrow
import Control.Monad (when)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (liftIO)
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Traversable
import System.Directory (doesFileExist, removeFile, renameFile)
import System.FilePath ((<.>))

-- | Traverse the given structure, retrieving values from and storing values in
-- the cache at the given path. Only the element type need be serializable. The
-- struture must be lazy in the values for this to be effective.
cached :: (Binary a, Traversable t) => FilePath -> t a -> IO (t a)
cached path dat = do
    cacheExists <- doesFileExist path
    cache <- if cacheExists then LBS.readFile path else return LBS.empty
    newExists <- doesFileExist newPath
    when newExists $ removeFile newPath
    as <- State.evalStateT (traverse cached_go dat) (cache, Seq.empty)
    return as
  where
    newPath = path <.> "new"

    appendEncoded = flip (|>) . LBS.toStrict . encode

    flushCache = do
        (oldCache, newCache) <- State.get
        when (LBS.null oldCache) $ liftIO $ do
            forM_ newCache (BS.appendFile newPath)
            renameFile newPath path

    cached_go computed = do
        decoded <- decodeOrFail . fst <$> State.get
        case decoded of
            Left _ -> do
                State.modify $ first $ const LBS.empty
                State.modify $ const LBS.empty *** appendEncoded computed
                seq computed flushCache
                return computed
            Right (nextCache, _, retrieved) -> do
                State.modify $ const nextCache *** appendEncoded retrieved
                seq retrieved flushCache
                return retrieved
