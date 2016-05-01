module Graphics.Urho3D.Graphics.Internal.AnimationState(
    AnimationState
  , animationStateCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data AnimationState

animationStateCntx :: C.Context 
animationStateCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "AnimationState", [t| AnimationState |])
    ]
  } 