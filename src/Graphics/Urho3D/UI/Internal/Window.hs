module Graphics.Urho3D.UI.Internal.Window(
    Window
  , windowCntx
  , sharedWindowPtrCntx
  , weakWindowPtrCntx
  , SharedWindow
  , WeakWindow 
  , WindowDragMode(..)
  ) where

import GHC.Generics
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

-- | Window UI element that can optionally by moved or resized.
data Window

windowCntx :: C.Context
windowCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Window", [t| Window |])
    ]
  }

sharedPtrImpl "Window"
sharedWeakPtrImpl "Window"

-- | Window movement and resizing modes.
data WindowDragMode =
    DragNone
  | DragMove
  | DragResizeTopLeft
  | DragResizeTop
  | DragResizeTopRight
  | DragResizeRight
  | DragResizeBottomRight
  | DragResizeBottom
  | DragResizeBottomLeft
  | DragResizeLeft
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
