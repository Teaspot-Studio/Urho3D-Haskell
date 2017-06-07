{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.RenderSurface(
    RenderSurface
  , renderSurfaceContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.RenderSurface
import Data.Monoid

C.context (C.cppCtx <> renderSurfaceCntx )
C.include "<Urho3D/Graphics/RenderSurface.h>"
C.using "namespace Urho3D"

renderSurfaceContext :: C.Context
renderSurfaceContext = renderSurfaceCntx
