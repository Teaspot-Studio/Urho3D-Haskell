module Graphics.Urho3D.UI.Internal.Window(
    Window 
  , windowCntx
  , sharedWindowPtrCntx
  , SharedWindow
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Window

windowCntx :: C.Context 
windowCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Window", [t| Window |])
    ]
  } 

sharedPtrImpl "Window"