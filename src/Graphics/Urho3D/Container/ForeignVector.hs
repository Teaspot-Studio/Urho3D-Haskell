module Graphics.Urho3D.Container.ForeignVector(
    ReadableVector(..)
  , WriteableVector(..)
  , ForeignVectorRepresent(..)
  ) where 

import Foreign 
import Control.DeepSeq 
import Control.Monad.IO.Class 
import Control.Monad.Catch 
import qualified Data.Vector as V
import qualified Data.Sequence as S

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

-- | Strict version of @foreignVectorAsList@
foreignVectorAsList' :: (MonadIO m, ReadableVector a) => Ptr a -> m [ReadVecElem a]
foreignVectorAsList' ptr = do 
  len <- foreignVectorLength ptr
  lst <- mapM (foreignVectorElement' ptr) [0 .. len-1]
  return $ length lst `seq` lst 

-- | Lazy loading of foreign vector
foreignVectorAsVector :: MonadIO m => ReadableVector a => Ptr a -> m (V.Vector (ReadVecElem a))
foreignVectorAsVector ptr = do 
  len <- foreignVectorLength ptr
  V.generateM len $ foreignVectorElement ptr

-- | Strict version of @foreignVectorAsVector@
foreignVectorAsVector' :: (MonadIO m, ReadableVector a, NFData (ReadVecElem a)) => Ptr a -> m (V.Vector (ReadVecElem a))
foreignVectorAsVector' ptr = do 
  len <- foreignVectorLength ptr
  vec <- V.generateM len $ foreignVectorElement ptr
  vec `deepseq` return vec 

-- | Lazy loading of foreign vector
foreignVectorAsSeq :: MonadIO m => ReadableVector a => Ptr a -> m (S.Seq (ReadVecElem a))
foreignVectorAsSeq ptr = do 
  len <- foreignVectorLength ptr
  sequence $ S.fromFunction len (foreignVectorElement ptr)

-- | Strict version of @foreignVectorAsSeq@
foreignVectorAsSeq' :: (MonadIO m, ReadableVector a, NFData (ReadVecElem a)) => Ptr a -> m (S.Seq (ReadVecElem a))
foreignVectorAsSeq' ptr = do 
  len <- foreignVectorLength ptr
  s <- sequence $ S.fromFunction len (foreignVectorElement ptr)
  s `deepseq` return s 

-- | Allows to define functions with return results of different representations
class ForeignVectorRepresent a where 
  -- | Peek vector to given representation
  peekForeignVectorAs :: (MonadIO m, ReadableVector v) => Ptr v -> m (a (ReadVecElem v))
  -- | Peek vector to given representation, strict version
  peekForeignVectorAs' :: (MonadIO m, ReadableVector v, NFData (ReadVecElem v)) => Ptr v -> m (a (ReadVecElem v))

  -- | Creates vector, fills it with list elements, runs action, deletes vector after action
  -- Note: take into account any lazy operations that uses the vector,
  --  outside the function should not be any operations with the vector
  withForeignVector :: (MonadIO m, MonadMask m, Createable (Ptr v), WriteableVector v) 
    => CreationOptions (Ptr v) -- ^ Specific options for vector creation
    -> (a (WriteVecElem v)) -- ^ Elements of the vector
    -> (Ptr v -> m b) -- ^ Handler
    -> m b -- ^ Result

  -- | Creates vector, fills it with list elements, runs action, deletes vector after action
  withForeignVector' :: (MonadIO m, MonadMask m, NFData b, Createable (Ptr v), WriteableVector v) 
    => CreationOptions (Ptr v) -- ^ Specific options for vector creation
    -> (a (WriteVecElem v)) -- ^ Elements of the vector
    -> (Ptr v -> m b) -- ^ Handler
    -> m b -- ^ Result

instance ForeignVectorRepresent [] where 
  peekForeignVectorAs = foreignVectorAsList
  peekForeignVectorAs' = foreignVectorAsList'
  withForeignVector opts es handler = withObject opts $ \v -> mapM (foreignVectorAppend v) es >> handler v
  withForeignVector' opts es handler = withObject' opts $ \v -> mapM (foreignVectorAppend v) es >> handler v

instance ForeignVectorRepresent V.Vector where 
  peekForeignVectorAs = foreignVectorAsVector 
  peekForeignVectorAs' = foreignVectorAsVector' 
  withForeignVector opts es handler = withObject opts $ \v -> mapM (foreignVectorAppend v) es >> handler v
  withForeignVector' opts es handler = withObject' opts $ \v -> mapM (foreignVectorAppend v) es >> handler v

instance ForeignVectorRepresent S.Seq where 
  peekForeignVectorAs = foreignVectorAsSeq
  peekForeignVectorAs' = foreignVectorAsSeq' 
  withForeignVector opts es handler = withObject opts $ \v -> mapM (foreignVectorAppend v) es >> handler v
  withForeignVector' opts es handler = withObject' opts $ \v -> mapM (foreignVectorAppend v) es >> handler v
