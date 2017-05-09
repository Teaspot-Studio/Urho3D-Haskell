{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Resource.Cache(
    ResourceCache
  , resourceCacheContext
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

-- | Loading a resource by name
cacheGetResource :: forall a m . (ResourceType a, MonadIO m) => Ptr ResourceCache
  -> String -- ^ Resource name
  -> Bool -- ^ send event on failure?
  -> m (Maybe (Ptr a)) -- ^ pointer to resource
cacheGetResource ptr name sendEvent = liftIO $ withCString name $ \name' -> do
  let rest = fromIntegral . stringHashValue $ resourceType (Proxy :: Proxy a)
  let sendEvent' = if sendEvent then 1 else 0
  resPtr <- [C.exp| Resource* { $(ResourceCache* ptr)->GetResource(StringHash($(unsigned int rest)), String($(const char* name')), $(int sendEvent') != 0) } |]
  checkNullPtr' resPtr (return.castPtr)
