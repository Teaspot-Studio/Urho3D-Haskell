{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.StaticModel(
    StaticModel
  , staticModelContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.StaticModel
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Graphics.Drawable 
import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Scene.Component

C.context (C.cppCtx <> staticModelCntx <> componentContext <> stringHashContext <> drawableContext)
C.include "<Urho3D/Graphics/StaticModel.h>"
C.using "namespace Urho3D"

staticModelContext :: C.Context 
staticModelContext = staticModelCntx <> componentContext <> stringHashContext

instance Parent Component StaticModel where 
  castToParent ptr = [C.pure| Component* { (Component*)$(StaticModel* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| StaticModel* { (StaticModel*)$(Component* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Parent Drawable StaticModel where 
  castToParent ptr = [C.pure| Drawable* { (Drawable*)$(StaticModel* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| StaticModel* { (StaticModel*)$(Drawable* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance IsComponent StaticModel where 
  componentHash _ = unsafePerformIO [C.block| StringHash* { 
    static StringHash* sh = NULL;
    if (!sh) {
      sh = new StringHash(StaticModel::GetTypeStatic());
    }
    return sh;
  } |]