module Graphics.Urho3D.Graphics.Internal.Material(
    Material
  , materialCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Material 

materialCntx :: C.Context 
materialCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Material", [t| Material |])
    ]
  } 