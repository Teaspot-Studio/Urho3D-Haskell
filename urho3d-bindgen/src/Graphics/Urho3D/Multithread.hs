module Graphics.Urho3D.Multithread(
    runInMainThread
  , UrhoHandler
  , addMainThreadWorker
  , runAllMainThreadCallbacks
  ) where

import Control.Concurrent.STM.TChan
import Control.Exception (handle, SomeException)
import Control.Monad.STM (atomically)
import Data.IntMap.Strict (IntMap)
import Data.IORef
import Graphics.Urho3D.Monad hiding (handle)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as IS

-- | Use this quque to pipe actions to be run in main thead
mainThreadCallbackQueue :: TChan (IO ())
mainThreadCallbackQueue = unsafePerformIO newTChanIO
{-# NOINLINE mainThreadCallbackQueue #-}

-- | Enqueue action to be run in main thread.
runInMainThread :: MonadIO m => IO () -> m ()
runInMainThread = liftIO . atomically . writeTChan mainThreadCallbackQueue

-- | Called by Urho, empties queue of callbacks and executes them.
runAllMainThreadCallbacks :: IO ()
runAllMainThreadCallbacks = do
  runUrhoHandlers
  ios <- atomically $ extractCallbacks []
  sequence_ . reverse $ ios
  where
  extractCallbacks acc = do
    mio <- tryReadTChan mainThreadCallbackQueue
    case mio of
      Nothing -> return acc
      Just io -> extractCallbacks (io : acc)

-- | IO action that is called on every frame in Urho main thread.
--
-- If the handler returns 'False' it won't be executed anymore.
type UrhoHandler = IO Bool

-- | Global registry of handlers
data UrhoHandlers = UrhoHandlers !Int !(IntMap UrhoHandler)

-- | Global var to that holds handlers
mainThreadHandlers :: IORef UrhoHandlers
mainThreadHandlers = unsafePerformIO $ newIORef $ UrhoHandlers 0 M.empty
{-# NOINLINE mainThreadHandlers #-}

-- | Adding the new main thread worker that is executed each frame
addMainThreadWorker :: UrhoHandler -> IO ()
addMainThreadWorker worker = atomicModifyIORef' mainThreadHandlers $ \(UrhoHandlers n oldhs) -> let
  newhs = M.insert n worker oldhs
  in (UrhoHandlers (n+1) newhs, ())

-- | Execute handler and catch exceptions that indicates that we need to kill the handler
execHandler :: UrhoHandler -> IO Bool
execHandler = handle f
  where
    f :: SomeException -> IO Bool
    f e = do
      putStrLn $ "Main thread worker died with exception: " ++ show e
      pure False

-- | Read current handlers and perform them
runUrhoHandlers :: IO ()
runUrhoHandlers = do
  UrhoHandlers _ hs <- readIORef mainThreadHandlers
  reses <- traverse execHandler hs
  let deleted = M.keysSet $ M.filter not reses
  unless (IS.null deleted) $ atomicModifyIORef' mainThreadHandlers $ \(UrhoHandlers n oldhs) -> let
    newhs = M.withoutKeys hs deleted 
    in (UrhoHandlers n newhs, ())
