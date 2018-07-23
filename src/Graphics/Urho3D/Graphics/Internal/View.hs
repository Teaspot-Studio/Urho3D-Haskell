module Graphics.Urho3D.Graphics.Internal.View(
    View
  , viewCntx
  , sharedViewPtrCntx
  , SharedView
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

-- | Rendering path definition. A sequence of commands (e.g. clear screen, draw objects with specific pass) that yields the scene rendering result.
data View

viewCntx :: C.Context
viewCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "View", [t| View |])
    ]
  }

sharedPtrImpl "View"
