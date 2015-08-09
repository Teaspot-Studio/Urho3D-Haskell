{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Resource.Cache(
    ResourceCache 
  , resourceCacheContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Resource.Internal.Cache
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Control.Monad.IO.Class
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> resourceCacheCntx <> contextContext)
C.include "<Urho3D/Resource/ResourceCache.h>"
C.using "namespace Urho3D"

resourceCacheContext :: C.Context 
resourceCacheContext = resourceCacheCntx

newResourceCache :: Ptr Context -> IO (Ptr ResourceCache)
newResourceCache ptr = [C.exp| ResourceCache* { new ResourceCache( $(Context* ptr) ) } |]

deleteResourceCache :: Ptr ResourceCache -> IO ()
deleteResourceCache ptr = [C.exp| void { delete $(ResourceCache* ptr) } |]

instance Createable ResourceCache where 
  type CreationOptions ResourceCache = Ptr Context 

  newObject = liftIO . newResourceCache
  deleteObject = liftIO . deleteResourceCache
