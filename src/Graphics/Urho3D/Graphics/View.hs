{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.View(
    View
  , SharedView
  , viewContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.View
import Data.Monoid

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context

C.context (C.cppCtx
  <> viewCntx
  <> sharedViewPtrCntx
  <> contextContext
  )

C.include "<Urho3D/Graphics/View.h>"
C.using "namespace Urho3D"

viewContext :: C.Context
viewContext = viewCntx
  <> sharedViewPtrCntx

sharedPtr "View"
