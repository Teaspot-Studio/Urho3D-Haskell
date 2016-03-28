module Graphics.Urho3D.Container.ForeignVector(
    ReadableVector(..)
  , WriteableVector(..)
  , foreignVectorAsList
  , foreignVectorAsList'
  , withForeignVector
  , withForeignVector'
  ) where 

import Foreign 
import Control.DeepSeq 
import Control.Monad.IO.Class 
import Control.Monad.Catch 

import Graphics.Urho3D.Createable 

-- | Foreign vector that we can read
class ReadableVector a where 
  type ReadVecElem a :: * 

  -- | Getting length of vector
  foreignVectorLength :: MonadIO m => Ptr a -> m Int 
  -- | Getting element by index
  -- Lazy version
  foreignVectorElement :: MonadIO m => Ptr a -> Int -> m (ReadVecElem a)

  -- | Getting element by index
  -- Strict version
  -- Note: default implementation forces only WNF, override if 
  -- you use complex types
  foreignVectorElement' :: MonadIO m => Ptr a -> Int -> m (ReadVecElem a)
  foreignVectorElement' ptr i = do 
    v <- foreignVectorElement ptr i 
    return $! v 

-- | Foreign vector that we can append
class WriteableVector a where 
  type WriteVecElem a :: * 

  -- | Appending new element at end of vector
  foreignVectorAppend :: MonadIO m => Ptr a -> WriteVecElem a -> m ()

-- | Lazy loading of foreign vector
foreignVectorAsList :: MonadIO m => ReadableVector a => Ptr a -> m [ReadVecElem a]
foreignVectorAsList ptr = do 
  len <- foreignVectorLength ptr
  mapM (foreignVectorElement ptr) [0 .. len-1]

-- | String version of @foreignVectorAsList@
foreignVectorAsList' :: (MonadIO m, ReadableVector a) => Ptr a -> m [ReadVecElem a]
foreignVectorAsList' ptr = do 
  len <- foreignVectorLength ptr
  lst <- mapM (foreignVectorElement' ptr) [0 .. len-1]
  return $ length lst `seq` lst 

-- | Creates vector, fills it with list elements, runs action, deletes vector after action
-- Note: take into account any lazy operations that uses the vector,
--  outside the function should not be any operations with the vector
withForeignVector :: (MonadIO m, MonadMask m, Createable (Ptr a), WriteableVector a) 
  => CreationOptions (Ptr a) -- ^ Specific options for vector creation
  -> [WriteVecElem a] -- ^ Elements of the vector
  -> (Ptr a -> m b) -- ^ Handler
  -> m b -- ^ Result
withForeignVector opts es handler = withObject opts $ \v -> mapM (foreignVectorAppend v) es >> handler v

-- | Creates vector, fills it with list elements, runs action, deletes vector after action
withForeignVector' :: (MonadIO m, MonadMask m, NFData b, Createable (Ptr a), WriteableVector a) 
  => CreationOptions (Ptr a) -- ^ Specific options for vector creation
  -> [WriteVecElem a] -- ^ Elements of the vector
  -> (Ptr a -> m b) -- ^ Handler
  -> m b -- ^ Result
withForeignVector' opts es handler = withObject' opts $ \v -> mapM (foreignVectorAppend v) es >> handler v