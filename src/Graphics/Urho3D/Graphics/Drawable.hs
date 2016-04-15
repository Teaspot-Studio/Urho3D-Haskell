{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Drawable(
    Drawable
  , drawableContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Drawable
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Scene.Node

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> drawableCntx <> componentContext <> stringHashContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/Graphics/Drawable.h>"
C.using "namespace Urho3D"

drawableContext :: C.Context 
drawableContext = drawableCntx <> componentContext <> stringHashContext

deriveParents [''Object, ''Serializable, ''Animatable, ''Component] ''Drawable

instance NodeComponent Drawable where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = Drawable::GetTypeStatic();
    return &h;
  } |]
