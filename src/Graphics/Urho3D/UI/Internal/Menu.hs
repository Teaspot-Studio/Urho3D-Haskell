module Graphics.Urho3D.UI.Internal.Menu(
    Menu
  , menuCntx
  , sharedMenuPtrCntx
  , SharedMenu
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Menu

menuCntx :: C.Context 
menuCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Menu", [t| Menu |])
    ]
  }

sharedPtrImpl "Menu"
