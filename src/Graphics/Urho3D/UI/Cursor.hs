{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.Cursor(
    Cursor
  , SharedCursor
  , WeakCursor
  , cursorContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.UI.Internal.Cursor
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx
  <> cursorCntx
  <> contextContext
  <> borderImageContext
  <> uiElementContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> sharedCursorPtrCntx
  <> weakCursorPtrCntx
  )

C.include "<Urho3D/UI/Cursor.h>"
C.using "namespace Urho3D"

cursorContext :: C.Context
cursorContext = cursorCntx
  <> borderImageContext
  <> sharedCursorPtrCntx
  <> uiElementContext
  <> weakCursorPtrCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Cursor

sharedPtr "Cursor"
sharedWeakPtr "Cursor"

newCursor :: Ptr Context -> IO (Ptr Cursor)
newCursor ptr = [C.exp| Cursor* { new Cursor( $(Context* ptr) ) } |]

deleteCursor :: Ptr Cursor -> IO ()
deleteCursor ptr = [C.exp| void { delete $(Cursor* ptr) } |]

instance Creatable (Ptr Cursor) where
  type CreationOptions (Ptr Cursor) = Ptr Context

  newObject = liftIO . newCursor
  deleteObject = liftIO . deleteCursor

instance UIElem Cursor where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Cursor::GetTypeStatic().Value() } |]
