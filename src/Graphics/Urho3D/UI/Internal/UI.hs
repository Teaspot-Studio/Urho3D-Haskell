module Graphics.Urho3D.UI.Internal.UI(
    UI
  , uiCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data UI 

uiCntx :: C.Context 
uiCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "UI", [t| UI |])
    ]
  } 