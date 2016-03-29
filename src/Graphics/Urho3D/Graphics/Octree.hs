{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Octree(
    Octree
  , Octant 
  , octreeContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Octree
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Math.StringHash 

C.context (C.cppCtx <> octreeCntx <> componentContext <> stringHashContext)
C.include "<Urho3D/Graphics/Octree.h>"
C.using "namespace Urho3D"

octreeContext :: C.Context 
octreeContext = octreeCntx <> componentContext <> stringHashContext

instance Parent Component Octree where 
  castToParent ptr = [C.pure| Component* { (Component*)$(Octree* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Octree* { (Octree*)$(Component* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Parent Octant Octree where 
  castToParent ptr = [C.pure| Octant* { (Octant*)$(Octree* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Octree* { (Octree*)$(Octant* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance IsComponent Octree where 
  componentHash _ = unsafePerformIO [C.block| StringHash* { 
    static StringHash* sh = NULL;
    if (!sh) {
      sh = new StringHash(Octree::GetTypeStatic());
    }
    return sh;
  } |]