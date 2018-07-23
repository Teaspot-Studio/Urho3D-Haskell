{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.RenderPath(
    RenderPath
  , SharedRenderPath
  , renderPathContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.RenderPath
import Data.Monoid

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context

C.context (C.cppCtx
  <> renderPathCntx
  <> sharedRenderPathPtrCntx
  <> contextContext
  )

C.include "<Urho3D/Core/Variant.h>"
C.include "<Urho3D/Graphics/RenderPath.h>"
C.using "namespace Urho3D"

renderPathContext :: C.Context
renderPathContext = renderPathCntx
  <> sharedRenderPathPtrCntx

sharedPtr "RenderPath"
