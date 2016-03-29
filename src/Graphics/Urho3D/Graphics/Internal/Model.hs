module Graphics.Urho3D.Graphics.Internal.Model(
    Model
  , modelCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Model 

modelCntx :: C.Context 
modelCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Model", [t| Model |])
    ]
  } 