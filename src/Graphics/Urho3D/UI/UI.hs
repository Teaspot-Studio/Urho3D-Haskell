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
  , uiGetRoot
  , uiGetRootModalElement
  , uiGetCursor
  , uiGetCursorPosition
  , uiGetElementAt
  , uiGetFocusElement
  , uiGetFrontElement
  , uiGetDragElements
  , uiGetNumDragElements
  , uiGetDragElement
  , uiGetClipboardText
  , uiGetDoubleClickInterval
  , uiGetDragBeginInterval
  , uiGetDragBeginDistance
  , uiGetDefaultToolTipDelay
  , uiGetMaxFontTextureSize
  , uiIsNonFocusedMouseWheel
  , uiGetUseSystemClipboard
  , uiGetUseScreenKeyboard
  , uiGetUseMutableGlyphs
  , uiGetForceAutoHint
  , uiHasModalElement
  , uiIsDragging
  , uiGetScale
  , uiGetCustomSize
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Foreign.C
import Graphics.Urho3D.Container.ForeignVector
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

C.verbatim "typedef Vector<UIElement*> VectorUIElementPtr;"

uiContext :: C.Context
uiContext = objectContext <> uiCntx

deriveParent ''Object ''UI

instance Subsystem UI where
  getSubsystemImpl ptr = [C.exp| UI* { $(Object* ptr)->GetSubsystem<UI>() } |]

-- | Returns root UI element
uiRoot :: (Pointer p a, Parent UI a, MonadIO m) => p -> m (Ptr UIElement)
uiRoot = uiGetRoot

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

-- | Return root UI element.
-- UIElement* GetRoot() const { return rootElement_; }
uiGetRoot :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m (Ptr UIElement)
uiGetRoot p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| UIElement* { $(UI* ptr)->GetRoot() } |]

-- | Return root modal element.
-- UIElement* GetRootModalElement() const { return rootModalElement_; }
uiGetRootModalElement :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m (Ptr UIElement)
uiGetRootModalElement p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| UIElement* { $(UI* ptr)->GetRootModalElement() } |]

-- | Return cursor.
-- Cursor* GetCursor() const { return cursor_; }
uiGetCursor :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m (Ptr Cursor)
uiGetCursor p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Cursor* { $(UI* ptr)->GetCursor() } |]

-- | Return cursor position.
-- IntVector2 GetCursorPosition() const;
uiGetCursorPosition :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m IntVector2
uiGetCursorPosition p = liftIO $ alloca $ \vptr -> do
  let ptr = parentPointer p
  [C.block| void { *$(IntVector2* vptr) = $(UI* ptr)->GetCursorPosition(); } |]
  peek vptr

-- | Return UI element at screen coordinates. By default returns only input-enabled elements.
-- UIElement* GetElementAt(const IntVector2& position, bool enabledOnly = true);
uiGetElementAt :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> IntVector2 -- ^ position
  -> Bool -- ^ Enabled only?
  -> m (Maybe (Ptr UIElement))
uiGetElementAt p v e = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
      e' = fromBool e
  wrapNullPtr <$> [C.exp| UIElement* { $(UI* ptr)->GetElementAt(*$(IntVector2* v'), $(int e') != 0) } |]

-- | Return focused element.
-- UIElement* GetFocusElement() const { return focusElement_; }
uiGetFocusElement :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m (Maybe (Ptr UIElement))
uiGetFocusElement p = liftIO $ do
  let ptr = parentPointer p
  wrapNullPtr <$> [C.exp| UIElement* { $(UI* ptr)->GetFocusElement() } |]

-- | Return topmost enabled root-level non-modal element.
-- UIElement* GetFrontElement() const;
uiGetFrontElement :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m (Maybe (Ptr UIElement))
uiGetFrontElement p = liftIO $ do
  let ptr = parentPointer p
  wrapNullPtr <$> [C.exp| UIElement* { $(UI* ptr)->GetFrontElement() } |]

-- | Return currently dragged elements.
-- const Vector<UIElement*> GetDragElements();
uiGetDragElements :: (Parent UI a, Pointer ptr a, MonadIO m, ForeignVector v (Ptr UIElement))
  => ptr -- ^ Pointer to UI or ascentor
  -> m (v (Ptr UIElement))
uiGetDragElements p = liftIO $ do
  let ptr = parentPointer p
  vptr <- [C.exp| VectorUIElementPtr* { new VectorUIElementPtr($(UI* ptr)->GetDragElements()) } |]
  v <- peekForeignVectorAs vptr
  [C.exp| void { delete $(VectorUIElementPtr* vptr) } |]
  pure v

-- | Return the number of currently dragged elements.
-- unsigned GetNumDragElements() const { return (unsigned)dragConfirmedCount_; }
uiGetNumDragElements :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Word
uiGetNumDragElements p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(UI* ptr)->GetNumDragElements() } |]

-- | Return the drag element at index.
-- UIElement* GetDragElement(unsigned index);
uiGetDragElement :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> Word -- ^ index
  -> m (Maybe (Ptr UIElement))
uiGetDragElement p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  wrapNullPtr <$> [C.exp| UIElement* { $(UI* ptr)->GetDragElement($(unsigned int v')) } |]

-- | Return clipboard text.
-- const String& GetClipboardText() const;
uiGetClipboardText :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m String
uiGetClipboardText p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* { $(UI* ptr)->GetClipboardText().CString() } |]

-- | Return UI element double click interval in seconds.
-- float GetDoubleClickInterval() const { return doubleClickInterval_; }
uiGetDoubleClickInterval :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Float
uiGetDoubleClickInterval p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(UI* ptr)->GetDoubleClickInterval() } |]

-- | Return UI drag start event interval in seconds.
-- float GetDragBeginInterval() const { return dragBeginInterval_; }
uiGetDragBeginInterval :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Float
uiGetDragBeginInterval p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(UI* ptr)->GetDragBeginInterval() } |]

-- | Return UI drag start event distance threshold in pixels.
-- int GetDragBeginDistance() const { return dragBeginDistance_; }
uiGetDragBeginDistance :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Int
uiGetDragBeginDistance p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UI* ptr)->GetDragBeginDistance() } |]

-- | Return tooltip default display delay in seconds.
-- float GetDefaultToolTipDelay() const { return defaultToolTipDelay_; }
uiGetDefaultToolTipDelay :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Float
uiGetDefaultToolTipDelay p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(UI* ptr)->GetDefaultToolTipDelay() } |]

-- | Return font texture maximum size.
-- int GetMaxFontTextureSize() const { return maxFontTextureSize_; }
uiGetMaxFontTextureSize :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Int
uiGetMaxFontTextureSize p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UI* ptr)->GetMaxFontTextureSize() } |]

-- | Return whether mouse wheel can control also a non-focused element.
-- bool IsNonFocusedMouseWheel() const { return nonFocusedMouseWheel_; }
uiIsNonFocusedMouseWheel :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Bool
uiIsNonFocusedMouseWheel p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UI* ptr)->IsNonFocusedMouseWheel() } |]

-- | Return whether is using the system clipboard.
-- bool GetUseSystemClipboard() const { return useSystemClipboard_; }
uiGetUseSystemClipboard :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Bool
uiGetUseSystemClipboard p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UI* ptr)->GetUseSystemClipboard() } |]

-- | Return whether focusing a %LineEdit will show the on-screen keyboard.
-- bool GetUseScreenKeyboard() const { return useScreenKeyboard_; }
uiGetUseScreenKeyboard :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Bool
uiGetUseScreenKeyboard p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UI* ptr)->GetUseScreenKeyboard() } |]

-- | Return whether is using mutable (eraseable) glyphs for fonts.
-- bool GetUseMutableGlyphs() const { return useMutableGlyphs_; }
uiGetUseMutableGlyphs :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Bool
uiGetUseMutableGlyphs p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UI* ptr)->GetUseMutableGlyphs() } |]

-- | Return whether is using forced autohinting.
-- bool GetForceAutoHint() const { return forceAutoHint_; }
uiGetForceAutoHint :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Bool
uiGetForceAutoHint p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UI* ptr)->GetForceAutoHint() } |]

-- | Return true when UI has modal element(s).
-- bool HasModalElement() const;
uiHasModalElement :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Bool
uiHasModalElement p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UI* ptr)->HasModalElement() } |]

-- | Return whether a drag is in progress.
-- bool IsDragging() const { return dragConfirmedCount_ > 0; };
uiIsDragging :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Bool
uiIsDragging p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UI* ptr)->IsDragging() } |]

-- | Return current UI scale.
-- float GetScale() const { return uiScale_; }
uiGetScale :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m Float
uiGetScale p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(UI* ptr)->GetScale() } |]

-- | Return root element custom size. Returns 0,0 when custom size is not being used and automatic resizing according to window size is in use instead (default.)
-- const IntVector2& GetCustomSize() const { return customSize_; }
uiGetCustomSize :: (Parent UI a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to UI or ascentor
  -> m IntVector2
uiGetCustomSize p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* { &$(UI* ptr)->GetCustomSize() } |]
