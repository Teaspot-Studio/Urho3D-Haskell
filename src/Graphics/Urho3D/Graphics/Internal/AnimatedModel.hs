module Graphics.Urho3D.Graphics.Internal.AnimatedModel(
    AnimatedModel
  , animatedModelCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data AnimatedModel

animatedModelCntx :: C.Context 
animatedModelCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "AnimatedModel", [t| AnimatedModel |])
    ]
  } 