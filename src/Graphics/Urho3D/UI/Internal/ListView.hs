module Graphics.Urho3D.UI.Internal.ListView(
    ListView
  , listViewCntx
  , sharedListViewPtrCntx
  , SharedListView
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data ListView

listViewCntx :: C.Context
listViewCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "ListView", [t| ListView |])
    ]
  }

sharedPtrImpl "ListView"
