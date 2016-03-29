{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Drawable(
    Drawable
  , drawableContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Drawable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Math.StringHash 

C.context (C.cppCtx <> drawableCntx <> componentContext <> stringHashContext)
C.include "<Urho3D/Graphics/Drawable.h>"
C.using "namespace Urho3D"

drawableContext :: C.Context 
drawableContext = drawableCntx <> componentContext <> stringHashContext

instance Parent Component Drawable where 
  castToParent ptr = [C.pure| Component* { (Component*)$(Drawable* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Drawable* { (Drawable*)$(Component* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance IsComponent Drawable where 
  componentHash _ = unsafePerformIO [C.block| StringHash* { 
    static StringHash* sh = NULL;
    if (!sh) {
      sh = new StringHash(Drawable::GetTypeStatic());
    }
    return sh;
  } |]