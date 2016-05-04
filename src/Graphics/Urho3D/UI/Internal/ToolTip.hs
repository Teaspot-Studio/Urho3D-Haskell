module Graphics.Urho3D.UI.Internal.ToolTip(
    ToolTip 
  , toolTipCntx
  , sharedToolTipPtrCntx
  , SharedToolTip
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data ToolTip

toolTipCntx :: C.Context 
toolTipCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "ToolTip", [t| ToolTip |])
    ]
  } 

sharedPtrImpl "ToolTip"