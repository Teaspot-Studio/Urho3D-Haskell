{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.Window(
    Window
  , windowContext
  , SharedWindow
  , WeakWindow
  , WindowDragMode(..)
  , windowSetMovable
  , windowSetResizable
  , windowSetFixedWidthResizing
  , windowSetFixedHeightResizing
  , windowSetResizeBorder
  , windowSetModal
  , windowSetModalShadeColor
  , windowSetModalFrameColor
  , windowSetModalFrameSize
  , windowSetModalAutoDismiss
  , windowIsMovable
  , windowIsResizable
  , windowGetFixedWidthResizing
  , windowGetFixedHeightResizing
  , windowGetResizeBorder
  , windowIsModal
  , windowGetModalShadeColor
  , windowGetModalFrameColor
  , windowGetModalFrameSize
  , windowGetModalAutoDismiss
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.Window
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx
  <> sharedWindowPtrCntx
  <> weakWindowPtrCntx
  <> windowCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> rectContext
  <> colorContext
  <> vector2Context
  )
C.include "<Urho3D/UI/Window.h>"
C.using "namespace Urho3D"

windowContext :: C.Context
windowContext = sharedWindowPtrCntx
  <> weakWindowPtrCntx
  <> windowCntx

instance Creatable (Ptr Window) where
  type CreationOptions (Ptr Window) = Ptr Context

  newObject ptr = liftIO $ [C.exp| Window* { new Window( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Window* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Window

instance UIElem Window where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Window::GetTypeStatic().Value() } |]

sharedPtr "Window"
sharedWeakPtr "Window"

-- | Set whether can be moved.
-- void SetMovable(bool enable);
windowSetMovable :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> Bool -- ^ enable
  -> m ()
windowSetMovable p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(Window* ptr)->SetMovable($(int v') != 0) } |]

-- | Set whether can be resized.
-- void SetResizable(bool enable);
windowSetResizable :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> Bool -- ^ enable
  -> m ()
windowSetResizable p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(Window* ptr)->SetResizable($(int v') != 0) } |]

-- | Set whether resizing width is fixed.
-- void SetFixedWidthResizing(bool enable);
windowSetFixedWidthResizing :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> Bool -- ^ enable
  -> m ()
windowSetFixedWidthResizing p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(Window* ptr)->SetFixedWidthResizing($(int v') != 0) } |]

-- | Set whether resizing height is fixed.
-- void SetFixedHeightResizing(bool enable);
windowSetFixedHeightResizing :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> Bool -- ^ enable
  -> m ()
windowSetFixedHeightResizing p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(Window* ptr)->SetFixedHeightResizing($(int v') != 0) } |]

-- | Set resize area width at edges.
-- void SetResizeBorder(const IntRect& rect);
windowSetResizeBorder :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> IntRect
  -> m ()
windowSetResizeBorder p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Window* ptr)->SetResizeBorder(*$(IntRect* v')) } |]

-- | Set modal flag. When the modal flag is set, the focused window needs to be dismissed first to allow other UI elements to gain focus.
-- void SetModal(bool modal);
windowSetModal :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> Bool -- ^ modal
  -> m ()
windowSetModal p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(Window* ptr)->SetModal($(int v') != 0) } |]

-- | Set modal shade color.
-- void SetModalShadeColor(const Color& color);
windowSetModalShadeColor :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> Color
  -> m ()
windowSetModalShadeColor p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Window* ptr)->SetModalShadeColor(*$(Color* v')) } |]

-- | Set modal frame color.
-- void SetModalFrameColor(const Color& color);
windowSetModalFrameColor :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> Color
  -> m ()
windowSetModalFrameColor p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Window* ptr)->SetModalFrameColor(*$(Color* v')) } |]

-- | Set modal frame size.
-- void SetModalFrameSize(const IntVector2& size);
windowSetModalFrameSize :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> IntVector2 -- ^ size
  -> m ()
windowSetModalFrameSize p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Window* ptr)->SetModalFrameSize(*$(IntVector2* v')) } |]

-- | Set whether model window can be dismissed with the escape key. Default true.
-- void SetModalAutoDismiss(bool enable);
windowSetModalAutoDismiss :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> Bool -- ^ enable
  -> m ()
windowSetModalAutoDismiss p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(Window* ptr)->SetModalAutoDismiss($(int v') != 0) } |]


-- | Return whether is movable.
-- bool IsMovable() const { return movable_; }
windowIsMovable :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m Bool
windowIsMovable p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Window* ptr)->IsMovable() } |]

-- | Return whether is resizable.
-- bool IsResizable() const { return resizable_; }
windowIsResizable :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m Bool
windowIsResizable p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Window* ptr)->IsResizable() } |]

-- | Return whether is resizing width is fixed.
-- bool GetFixedWidthResizing() const { return fixedWidthResizing_; }
windowGetFixedWidthResizing :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m Bool
windowGetFixedWidthResizing p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Window* ptr)->GetFixedWidthResizing() } |]

-- | Return whether is resizing height is fixed.
-- bool GetFixedHeightResizing() const { return fixedHeightResizing_; }
windowGetFixedHeightResizing :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m Bool
windowGetFixedHeightResizing p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Window* ptr)->GetFixedHeightResizing() } |]

-- | Return resize area width at edges.
-- const IntRect& GetResizeBorder() const { return resizeBorder_; }
windowGetResizeBorder :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m IntRect
windowGetResizeBorder p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntRect* { &$(Window* ptr)->GetResizeBorder() } |]

-- | Return modal flag.
-- bool IsModal() const { return modal_; }
windowIsModal :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m Bool
windowIsModal p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Window* ptr)->IsModal() } |]

-- | Get modal shade color.
-- const Color& GetModalShadeColor() const { return modalShadeColor_; }
windowGetModalShadeColor :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m Color
windowGetModalShadeColor p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Color* { &$(Window* ptr)->GetModalShadeColor() } |]

-- | Get modal frame color.
-- const Color& GetModalFrameColor() const { return modalFrameColor_; }
windowGetModalFrameColor :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m Color
windowGetModalFrameColor p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Color* { &$(Window* ptr)->GetModalFrameColor() } |]

-- | Get modal frame size.
-- const IntVector2& GetModalFrameSize() const { return modalFrameSize_; }
windowGetModalFrameSize :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m IntVector2
windowGetModalFrameSize p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* { &$(Window* ptr)->GetModalFrameSize() } |]

-- | Return whether can be dismissed with escape key.
-- bool GetModalAutoDismiss() const { return modalAutoDismiss_; }
windowGetModalAutoDismiss :: (Parent Window a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Window' or ascentor
  -> m Bool
windowGetModalAutoDismiss p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Window* ptr)->GetModalAutoDismiss() } |]
