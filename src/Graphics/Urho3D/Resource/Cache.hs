{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Resource.Cache(
    ResourceCache
  , resourceCacheContext
  , priorityLast
  , cacheAddResourceDir
  , cacheGetResource
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Resource.Internal.Cache
import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Data.Proxy
import Foreign
import Foreign.C.String

C.context (C.cppCtx <> resourceCacheCntx <> contextContext <> resourceContext <> objectContext)
C.include "<Urho3D/Resource/ResourceCache.h>"
C.using "namespace Urho3D"

resourceCacheContext :: C.Context
resourceCacheContext = resourceCacheCntx <> resourceContext <> objectContext

newResourceCache :: Ptr Context -> IO (Ptr ResourceCache)
newResourceCache ptr = [C.exp| ResourceCache* { new ResourceCache( $(Context* ptr) ) } |]

deleteResourceCache :: Ptr ResourceCache -> IO ()
deleteResourceCache ptr = [C.exp| void { delete $(ResourceCache* ptr) } |]

instance Creatable (Ptr ResourceCache) where
  type CreationOptions (Ptr ResourceCache) = Ptr Context

  newObject = liftIO . newResourceCache
  deleteObject = liftIO . deleteResourceCache

instance Subsystem ResourceCache where
  getSubsystemImpl ptr = [C.exp| ResourceCache* { $(Object* ptr)->GetSubsystem<ResourceCache>() } |]

-- | Default periority for resource cache
priorityLast :: Word
priorityLast = fromIntegral [C.pure| unsigned int { PRIORITY_LAST } |]

-- | Add a resource load directory. Optional priority parameter which will control search order.
cacheAddResourceDir :: forall a m ptr . (Parent ResourceCache a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to ResourceCache or acenstor
  -> FilePath -- ^ Path to directory
  -> Word -- ^ Priority (default is priorityLast)
  -> m Bool
cacheAddResourceDir ptr pathName priority = liftIO $ withCString pathName $ \pathName' -> do
  let ptr' = parentPointer ptr
      priority' = fromIntegral priority
  toBool <$> [C.exp| int { (int)$(ResourceCache* ptr')->AddResourceDir(String($(const char* pathName')), $(unsigned int priority')) } |]

-- | Loading a resource by name
cacheGetResource :: forall a b m ptr . (Parent ResourceCache a, Pointer ptr a, ResourceType b, MonadIO m)
  => ptr -- ^ Pointer to ResourceCache or acenstor
  -> String -- ^ Resource name
  -> Bool -- ^ send event on failure?
  -> m (Maybe (Ptr b)) -- ^ pointer to resource
cacheGetResource ptr name sendEvent = liftIO $ withCString name $ \name' -> do
  let ptr' = parentPointer ptr
      rest = fromIntegral . stringHashValue $ resourceType (Proxy :: Proxy b)
      sendEvent' = if sendEvent then 1 else 0
  resPtr <- [C.exp| Resource* { $(ResourceCache* ptr')->GetResource(StringHash($(unsigned int rest)), String($(const char* name')), $(int sendEvent') != 0) } |]
  checkNullPtr' resPtr (return.castPtr)
