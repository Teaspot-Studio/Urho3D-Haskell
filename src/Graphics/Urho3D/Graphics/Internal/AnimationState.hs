module Graphics.Urho3D.Graphics.Internal.AnimationState(
    AnimationState
  , animationStateCntx
  , SharedAnimationState
  , SharedAnimationStatePtr(..)
  , sharedAnimationStatePtrCntx
  , VectorSharedAnimationStatePtr
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data AnimationState
data VectorSharedAnimationStatePtr

animationStateCntx :: C.Context 
animationStateCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "AnimationState", [t| AnimationState |])
    , (C.TypeName "VectorSharedAnimationStatePtr", [t| VectorSharedAnimationStatePtr |])
    ]
  } 

sharedPtrImpl "AnimationState"