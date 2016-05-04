module Graphics.Urho3D.Graphics.Internal.Viewport(
    Viewport 
  , viewportCntx
  , sharedViewportPtrCntx
  , SharedViewport
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr 
import qualified Data.Map as Map

data Viewport

viewportCntx :: C.Context 
viewportCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Viewport", [t| Viewport |])
    ]
  }

sharedPtrImpl "Viewport"