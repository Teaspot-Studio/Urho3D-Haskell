module Graphics.Urho3D.UI.Internal.ScrollBar(
    ScrollBar
  , scrollBarCntx
  , sharedScrollBarPtrCntx
  , SharedScrollBar
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data ScrollBar

scrollBarCntx :: C.Context 
scrollBarCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "ScrollBar", [t| ScrollBar |])
    ]
  }

sharedPtrImpl "ScrollBar"
