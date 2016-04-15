{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Octree(
    Octree
  , Octant 
  , octreeContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Octree
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Scene.Node

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> octreeCntx <> componentContext <> stringHashContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/Graphics/Octree.h>"
C.using "namespace Urho3D"

octreeContext :: C.Context 
octreeContext = octreeCntx <> componentContext <> stringHashContext

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Octant] ''Octree
deriveParents [''Object, ''Serializable, ''Animatable, ''Component] ''Octant

instance NodeComponent Octree where 
  nodeComponentType _ = unsafePerformIO [C.block| StringHash* { 
    static StringHash sh = Octree::GetTypeStatic();
    return &sh;
  } |]