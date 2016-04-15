{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Material(
    Material
  , materialContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Material
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> materialCntx <> resourceContext <> objectContext)
C.include "<Urho3D/Graphics/Material.h>"
C.using "namespace Urho3D"

materialContext :: C.Context 
materialContext = materialCntx <> resourceContext

deriveParents [''Object, ''Resource] ''Material

instance ResourceType Material where 
  resourceType _ = unsafePerformIO $ [C.block| StringHash* { 
    static StringHash h = Material::GetTypeStatic(); 
    return &h; 
    } |]