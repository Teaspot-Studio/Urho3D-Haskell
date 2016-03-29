module Graphics.Urho3D.Graphics.Internal.StaticModel(
    StaticModel
  , staticModelCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data StaticModel

staticModelCntx :: C.Context 
staticModelCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "StaticModel", [t| StaticModel |])
    ]
  } 