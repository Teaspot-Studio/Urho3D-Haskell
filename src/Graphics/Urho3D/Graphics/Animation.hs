{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Animation(
    Animation
  , animationContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Animation
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent

C.context (C.cppCtx 
  <> sharedAnimationPtrCntx 
  <> animationCntx 
  <> resourceContext 
  <> objectContext
  <> contextContext)
C.include "<Urho3D/Graphics/Animation.h>"
C.using "namespace Urho3D"

animationContext :: C.Context 
animationContext = animationCntx <> resourceContext <> sharedAnimationPtrCntx

instance Createable (Ptr Animation) where 
  type CreationOptions (Ptr Animation) = Ptr Context 

  newObject ptr = liftIO [C.exp| Animation* { new Animation($(Context* ptr)) } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(Animation* ptr) } |]

sharedPtr "Animation"
deriveParents [''Object, ''Resource] ''Animation

instance ResourceType Animation where 
  resourceType _ = unsafePerformIO $ [C.block| StringHash* { 
    static StringHash h = Animation::GetTypeStatic(); 
    return &h; 
    } |]
