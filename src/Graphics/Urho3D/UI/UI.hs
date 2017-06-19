{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.UI(
    UI
  , uiContext
  , uiRoot
  , uiFocusElement
  , uiCursor
  , uiSetCursor
  , uiSetFocusElement
  , uiSetModalElement
  , uiClear
  , uiSetClipboardText
  , uiSetDoubleClickInterval
  , uiSetDragBeginInterval
  , uiSetDragBeginDistance
  , uiSetDefaultToolTipDelay
  , uiSetMaxFontTextureSize
  , uiSetNonFocusedMouseWheel
  , uiSetUseSystemClipboard
  , uiSetUseScreenKeyboard
  , uiSetUseMutableGlyphs
  , uiSetForceAutoHint
  , uiSetScale
  , uiSetWidth
  , uiSetHeight
  , uiSetCustomSize
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Foreign.C
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.UI.Cursor
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.Internal.UI

C.context (C.cppCtx <> uiCntx <> objectContext <> uiElementContext <> cursorContext <> vector2Context)
C.include "<Urho3D/UI/UI.h>"
C.using "namespace Urho3D"

uiContext :: C.Context
uiContext = objectContext <> uiCntx

deriveParent ''Object ''UI

instance Subsystem UI where
  getSubsystemImpl ptr = [C.exp| UI* { $(Object* ptr)->GetSubsystem<UI>() } |]

-- | Returns root UI element
uiRoot :: (Pointer p a, Parent UI a, MonadIO m) => p -> m (Ptr UIElement)
uiRoot ptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| UIElement* { $(UI* ptr')->GetRoot() } |]

-- | Returns current element in focus
uiFocusElement :: (Pointer p a, Parent UI a, MonadIO m) => p -> m (Maybe (Ptr UIElement))
uiFocusElement ptr = liftIO $ do
  let ptr' = parentPointer ptr
  fe <- [C.exp| UIElement* { $(UI* ptr')->GetFocusElement() } |]
  checkNullPtr' fe return

-- | Returns ui cursor
uiCursor :: (Pointer p a, Parent UI a, MonadIO m) => p -> m (Ptr Cursor)
uiCursor p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Cursor* { $(UI* ptr)->GetCursor() } |]

-- | Set cursor UI element.
uiSetCursor :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Ptr Cursor
  -> m ()
uiSetCursor p cursor = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UI* ptr)->SetCursor($(Cursor* cursor)) } |]

-- | Set focused UI element.
uiSetFocusElement :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Ptr UIElement
  -> Bool -- ^ by key (default False)
  -> m ()
uiSetFocusElement p element byKey = liftIO $ do
  let ptr = parentPointer p
      byKey' = fromBool byKey
  [C.exp| void { $(UI* ptr)->SetFocusElement($(UIElement* element), $(int byKey') != 0) } |]

-- | Set modal element. Until all the modal elements are dismissed, all the inputs and events are only sent to them. Return true when successful.
-- Only the modal element can clear its modal status or when it is being destructed.
uiSetModalElement :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Ptr UIElement
  -> Bool -- ^ enable
  -> m ()
uiSetModalElement p element enable = liftIO $ do
  let ptr = parentPointer p
      enable' = fromBool enable
  [C.exp| void { $(UI* ptr)->SetModalElement($(UIElement* element), $(int enable') != 0) } |]

-- | Clear the UI (excluding the cursor.)
uiClear :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> m ()
uiClear p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UI* ptr)->Clear() } |]

-- | Set clipboard text.
uiSetClipboardText :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> String
  -> m ()
uiSetClipboardText p str = liftIO $ withCString str $ \str' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UI* ptr)->SetClipboardText(String($(const char* str'))) } |]

-- | Set UI element double click interval in seconds.
uiSetDoubleClickInterval :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Float -- ^ interval
  -> m ()
uiSetDoubleClickInterval p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(UI* ptr)->SetDoubleClickInterval($(float v')) } |]

-- | Set UI drag event start interval in seconds.
uiSetDragBeginInterval :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Float -- ^ interval
  -> m ()
uiSetDragBeginInterval p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(UI* ptr)->SetDragBeginInterval($(float v')) } |]

-- | Set UI drag event start distance threshold in pixels.
uiSetDragBeginDistance :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Int -- ^ pixels
  -> m ()
uiSetDragBeginDistance p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(UI* ptr)->SetDragBeginDistance($(int v')) } |]

-- | Set tooltip default display delay in seconds.
uiSetDefaultToolTipDelay :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Float -- ^ delay
  -> m ()
uiSetDefaultToolTipDelay p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(UI* ptr)->SetDefaultToolTipDelay($(float v')) } |]

-- | Set maximum font face texture size. Must be a power of two. Default is 2048.
uiSetMaxFontTextureSize :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Int -- ^ size
  -> m ()
uiSetMaxFontTextureSize p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(UI* ptr)->SetMaxFontTextureSize($(int v')) } |]

-- | Set whether mouse wheel can control also a non-focused element.
uiSetNonFocusedMouseWheel :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Bool -- ^ enable
  -> m ()
uiSetNonFocusedMouseWheel p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(UI* ptr)->SetNonFocusedMouseWheel($(int v') != 0) } |]

-- | Set whether to use system clipboard. Default false.
uiSetUseSystemClipboard :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Bool -- ^ enable
  -> m ()
uiSetUseSystemClipboard p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(UI* ptr)->SetUseSystemClipboard($(int v') != 0) } |]

-- | Set whether to show the on-screen keyboard (if supported) when a %LineEdit is focused. Default true on mobile devices.
uiSetUseScreenKeyboard :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Bool -- ^ enable
  -> m ()
uiSetUseScreenKeyboard p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(UI* ptr)->SetUseScreenKeyboard($(int v') != 0) } |]

-- | Set whether to use mutable (eraseable) glyphs to ensure a font face never expands to more than one texture. Default false.
uiSetUseMutableGlyphs :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Bool -- ^ enable
  -> m ()
uiSetUseMutableGlyphs p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(UI* ptr)->SetUseMutableGlyphs($(int v') != 0) } |]

-- | Set whether to force font autohinting instead of using FreeType's TTF bytecode interpreter.
uiSetForceAutoHint :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Bool -- ^ enable
  -> m ()
uiSetForceAutoHint p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(UI* ptr)->SetForceAutoHint($(int v') != 0) } |]

-- | Set %UI scale. 1.0 is default (pixel perfect). Resize the root element to match.
uiSetScale :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Float -- ^ delay
  -> m ()
uiSetScale p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(UI* ptr)->SetScale($(float v')) } |]

-- | Scale %UI to the specified width in pixels.
uiSetWidth :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Float -- ^ delay
  -> m ()
uiSetWidth p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(UI* ptr)->SetWidth($(float v')) } |]

-- | Scale %UI to the specified height in pixels.
uiSetHeight :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> Float -- ^ delay
  -> m ()
uiSetHeight p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(UI* ptr)->SetHeight($(float v')) } |]

-- | Set custom size of the root element. This disables automatic resizing of the root element according to window size. Set custom size 0,0 to return to automatic resizing.
uiSetCustomSize :: (Pointer p a, Parent UI a, MonadIO m)
  => p -- ^ Pointer to UI object
  -> IntVector2 -- ^ size
  -> m ()
uiSetCustomSize p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UI* ptr)->SetCustomSize(*$(IntVector2* v')) } |]
