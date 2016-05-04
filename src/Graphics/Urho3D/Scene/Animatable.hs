{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Animatable(
    Animatable
  , animatableContext
  , SharedAnimatable
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Scene.Internal.Animatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Math.StringHash
import Data.Monoid

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Parent 

C.context (C.cppCtx <> animatableCntx <> sharedAnimatablePtrCntx <> contextContext <> stringHashContext <> serializableContext <> objectContext)
C.include "<Urho3D/Scene/Animatable.h>"
C.using "namespace Urho3D" 

animatableContext :: C.Context 
animatableContext = sharedAnimatablePtrCntx <> animatableCntx <> stringHashContext

sharedPtr "Animatable"

deriveParents [''Object, ''Serializable] ''Animatable