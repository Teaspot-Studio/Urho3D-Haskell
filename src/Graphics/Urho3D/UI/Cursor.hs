{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Cursor(
    Cursor
  , cursorContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Cursor
import Data.Monoid

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> cursorCntx <> borderImageContext <> uiElementContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/UI/Cursor.h>"
C.using "namespace Urho3D"

cursorContext :: C.Context 
cursorContext = cursorCntx <> borderImageContext <> uiElementContext

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Cursor