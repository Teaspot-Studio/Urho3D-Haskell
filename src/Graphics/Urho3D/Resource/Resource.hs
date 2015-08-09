{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Resource.Resource(
    Resource 
  , resourceContext
  , ResourceType(..)
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Resource.Internal.Resource
import Graphics.Urho3D.Math.StringHash
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> resourceCntx <> stringHashContext)
C.include "<Urho3D/Resource/Resource.h>"
C.using "namespace Urho3D"

resourceContext :: C.Context 
resourceContext = resourceCntx <> stringHashContext

-- | Types that could be loaded via resource cache
class ResourceType a where 
  resourceType :: a -> Ptr StringHash