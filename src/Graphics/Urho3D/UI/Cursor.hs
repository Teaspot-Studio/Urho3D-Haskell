{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.Cursor(
    Cursor
  , cursorContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Graphics.Urho3D.UI.Internal.Cursor
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx <> cursorCntx <> borderImageContext <> uiElementContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/UI/Cursor.h>"
C.using "namespace Urho3D"

cursorContext :: C.Context
cursorContext = cursorCntx <> borderImageContext <> uiElementContext

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Cursor

instance UIElem Cursor where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Cursor::GetTypeStatic().Value() } |]
