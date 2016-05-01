{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.AnimationState(
    AnimationState
  , animationStateContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.AnimationState
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import Foreign.C.String 
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Graphics.Model 
import Graphics.Urho3D.Graphics.Material
import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Scene.Node

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Graphics.Drawable 
import Graphics.Urho3D.Graphics.StaticModel
import Graphics.Urho3D.Parent

C.context (C.cppCtx 
  <> animationStateCntx)

C.include "<Urho3D/Graphics/AnimationState.h>"
C.using "namespace Urho3D"

animationStateContext :: C.Context 
animationStateContext = animationStateCntx 