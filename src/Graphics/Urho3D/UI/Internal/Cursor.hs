module Graphics.Urho3D.UI.Internal.Cursor(
    Cursor 
  , cursorCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data Cursor

cursorCntx :: C.Context 
cursorCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Cursor", [t| Cursor |])
    ]
  } 