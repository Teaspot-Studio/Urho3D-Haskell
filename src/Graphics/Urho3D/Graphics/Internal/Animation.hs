module Graphics.Urho3D.Graphics.Internal.Animation(
    Animation 
  , animationCntx
  , sharedAnimationPtrCntx
  , SharedAnimation
  , SharedAnimationPtr(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr 
import qualified Data.Map as Map

data Animation

animationCntx :: C.Context 
animationCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Animation", [t| Animation |])
    ]
  }

sharedPtrImpl "Animation"