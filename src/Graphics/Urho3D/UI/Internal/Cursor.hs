module Graphics.Urho3D.UI.Internal.Cursor(
    Cursor
  , SharedCursor
  , WeakCursor
  , cursorCntx
  , sharedCursorPtrCntx
  , weakCursorPtrCntx
  ) where

import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

data Cursor

cursorCntx :: C.Context
cursorCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Cursor", [t| Cursor |])
    ]
  }

sharedPtrImpl "Cursor"
sharedWeakPtrImpl "Cursor"
