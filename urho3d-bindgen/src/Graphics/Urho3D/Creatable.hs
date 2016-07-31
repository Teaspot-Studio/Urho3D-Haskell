module Graphics.Urho3D.Creatable(
    Creatable(..)
  , withObject
  , withObject'
  , deleteAfter
  , deleteAfter'
  ) where

import Control.Monad.IO.Class 
import Control.Monad.Catch
import Control.DeepSeq

-- | Abstraction of things that you can create and delete
class Creatable a where 
  type CreationOptions a :: *

  -- | Creates new object of type a using provided parameters specific for the object
  newObject :: MonadIO m => CreationOptions a -> m a
  -- | Descructs object, after the operation you haven't to use it
  deleteObject :: MonadIO m => a -> m ()

-- | Creates object, passes it to provided handler and then deletes it
-- Object would be deleted even on exception
-- Warning: Lazy function, take shure the object isn't used after the deletion, use @withObject'@ for strict return values
withObject :: (MonadIO m, MonadMask m, Creatable a) => CreationOptions a -> (a -> m b) -> m b
withObject opts = bracket (newObject opts) deleteObject

-- | Strict version of @withObject@ that forces result normal form before deleting of object
withObject' :: (MonadIO m, MonadMask m, Creatable a, NFData b) => CreationOptions a -> (a -> m b) -> m b
withObject' opts f = bracket (newObject opts) deleteObject $ \o -> do 
  v <- f o 
  return $!! v 

-- | Ensures that pointer is removed after IO action
deleteAfter :: (MonadIO m, MonadMask m, Creatable a) => a -> (a -> m b) -> m b 
deleteAfter ptr = bracket (return ptr) (liftIO . deleteObject)

-- | Strict version of @deleteAfter@, ensures that result of handler is forced to normal form
deleteAfter' :: (MonadIO m, MonadMask m, NFData b, Creatable a) => a -> (a -> m b) -> m b 
deleteAfter' ptr handler = bracket (return ptr) (liftIO . deleteObject) $ \ptr' -> do 
  res <- handler ptr'
  return $!! res 