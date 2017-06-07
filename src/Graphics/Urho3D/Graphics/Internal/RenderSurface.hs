module Graphics.Urho3D.Graphics.Internal.RenderSurface(
    RenderSurface
  , renderSurfaceCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data RenderSurface

renderSurfaceCntx :: C.Context
renderSurfaceCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "RenderSurface", [t| RenderSurface |])
    ]
  }
