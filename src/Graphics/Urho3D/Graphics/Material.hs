{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Material(
    Material
  , SharedMaterial
  , materialContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Material
import Data.Monoid
import Foreign

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx <> materialCntx <> resourceContext <> objectContext  <> sharedMaterialPtrCntx <> contextContext)
C.include "<Urho3D/Graphics/Material.h>"
C.using "namespace Urho3D"

materialContext :: C.Context
materialContext = materialCntx <> resourceContext <> sharedMaterialPtrCntx

deriveParents [''Object, ''Resource] ''Material

instance ResourceType Material where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Material::GetTypeStatic().Value() } |]

instance Creatable (Ptr Material) where
  type CreationOptions (Ptr Material) = Ptr Context
  newObject ptr = liftIO [C.exp| Material* {new Material($(Context* ptr))} |]
  deleteObject ptr = liftIO [C.exp| void {delete $(Material* ptr)} |]

sharedPtr "Material"
