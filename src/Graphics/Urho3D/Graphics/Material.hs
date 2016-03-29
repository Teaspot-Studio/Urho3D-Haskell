{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Material(
    Material
  , materialContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Material
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx <> materialCntx <> resourceContext)
C.include "<Urho3D/Graphics/Material.h>"
C.using "namespace Urho3D"

materialContext :: C.Context 
materialContext = materialCntx <> resourceContext

instance Parent Resource Material where 
  castToParent ptr = [C.pure| Resource* { (Resource*)$(Material* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Material* { (Material*)$(Resource* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance ResourceType Material where 
  resourceType _ = unsafePerformIO $ [C.block| StringHash* { 
    static StringHash h = Material::GetTypeStatic(); 
    return &h; 
    } |]