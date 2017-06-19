module Graphics.Urho3D.UI.Internal.Cursor(
    Cursor
  , SharedCursor
  , WeakCursor
  , CursorShape(..)
  , CursorShapeInfo(..)
  , cursorCntx
  , sharedCursorPtrCntx
  , weakCursorPtrCntx
  , HasImage(..)
  , HasTexture(..)
  , HasImageRect(..)
  , HasHotSpot(..)
  , HasOsCursor(..)
  , HasSystemDefined(..)
  , HasSystemCursor(..)
  ) where

import Control.Lens
import Foreign
import GHC.Generics
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Graphics.Internal.Texture
import Graphics.Urho3D.Math.Internal.Rect
import Graphics.Urho3D.Math.Internal.Vector2
import Graphics.Urho3D.Resource.Internal.Image
import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

data Cursor

-- | Cursor image and hotspot information.
data CursorShapeInfo = CursorShapeInfo {
  _cursorShapeInfoImage :: {-# UNPACK #-} !(SharedPtr Image) -- ^ Image.
, _cursorShapeInfoTexture :: {-# UNPACK #-} !(SharedPtr Texture) -- ^ Texture.
, _cursorShapeInfoImageRect :: {-# UNPACK #-} !IntRect -- ^ Image rectangle.
, _cursorShapeInfoHotSpot :: {-# UNPACK #-} !IntVector2 -- ^ Hotspot coordinates.
, _cursorShapeInfoOsCursor :: {-# UNPACK #-} !(Ptr ()) -- ^ OS cursor.
, _cursorShapeInfoSystemDefined :: !Bool -- ^ Whether the OS cursor is system defined.
, _cursorShapeInfoSystemCursor :: {-# UNPACK #-} !Int -- ^ System cursor index.
} deriving (Generic)
makeFields ''CursorShapeInfo

cursorCntx :: C.Context
cursorCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Cursor", [t| Cursor |])
    , (C.TypeName "CursorShapeInfo", [t| CursorShapeInfo |])
    ]
  }

sharedPtrImpl "Cursor"
sharedWeakPtrImpl "Cursor"

-- | Cursor shapes recognized by the UI subsystem.
data CursorShape =
    CursorNormal
  | CursorIBeam
  | CursorCross
  | CursorResizeVertical
  | CursorResizeDiagonalTopRight
  | CursorResizeHorizontal
  | CursorResizeDiagonalTopLeft
  | CursorResizeAll
  | CursorAcceptDrop
  | CursorRejectDrop
  | CursorBusy
  | CursorBusyArrow
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
