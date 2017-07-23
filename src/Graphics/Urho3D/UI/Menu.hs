{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Menu(
    Menu
  , menuContext
  , SharedMenu
  , menuSetPopup
  , menuSetPopupOffset
  , menuShowPopup
  , menuSetAccelerator
  , menuGetPopup
  , menuGetPopupOffset
  , menuGetShowPopup
  , menuGetAcceleratorKey
  , menuGetAcceleratorQualifiers
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Menu
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx
  <> sharedMenuPtrCntx
  <> menuCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> vector2Context)
C.include "<Urho3D/UI/Menu.h>"
C.using "namespace Urho3D"

menuContext :: C.Context
menuContext = sharedMenuPtrCntx
  <> menuCntx

instance Creatable (Ptr Menu) where
  type CreationOptions (Ptr Menu) = Ptr Context

  newObject ptr = liftIO $ [C.exp| Menu* { new Menu( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Menu* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Menu

instance UIElem Menu where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Menu::GetTypeStatic().Value() } |]

sharedPtr "Menu"

-- | Set popup element to show on selection.
-- void SetPopup(UIElement* element);
menuSetPopup :: (Parent Menu a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Menu or ascentor
  -> Ptr UIElement
  -> m ()
menuSetPopup p v = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(Menu* ptr)->SetPopup($(UIElement* v))} |]

-- | Set popup element offset.
-- void SetPopupOffset(const IntVector2& offset);
menuSetPopupOffset :: (Parent Menu a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Menu or ascentor
  -> IntVector2 -- ^ offset
  -> m ()
menuSetPopupOffset p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Menu* ptr)->SetPopupOffset(*$(IntVector2* v'))} |]

-- | Force the popup to show or hide.
-- void ShowPopup(bool enable);
menuShowPopup :: (Parent Menu a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Menu or ascentor
  -> Bool -- ^ enable
  -> m ()
menuShowPopup p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(Menu* ptr)->ShowPopup($(int v') != 0)} |]

-- | Set accelerator key (set zero key code to disable.)
-- void SetAccelerator(int key, int qualifiers);
menuSetAccelerator :: (Parent Menu a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Menu or ascentor
  -> Int -- ^ key
  -> Int -- ^ qualifiers
  -> m ()
menuSetAccelerator p k q = liftIO $ do
  let ptr = parentPointer p
      k' = fromIntegral k
      q' = fromIntegral q
  [C.exp| void {$(Menu* ptr)->SetAccelerator($(int k'), $(int q'))} |]

-- | Return popup element.
-- UIElement* GetPopup() const { return popup_; }
menuGetPopup :: (Parent Menu a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Menu or ascentor
  -> m (Ptr UIElement)
menuGetPopup p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| UIElement* {$(Menu* ptr)->GetPopup()} |]

-- | Return popup element offset.
-- const IntVector2& GetPopupOffset() const { return popupOffset_; }
menuGetPopupOffset :: (Parent Menu a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Menu or ascentor
  -> m IntVector2
menuGetPopupOffset p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* {&$(Menu* ptr)->GetPopupOffset()} |]

-- | Return whether popup is open.
-- bool GetShowPopup() const { return showPopup_; }
menuGetShowPopup :: (Parent Menu a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Menu or ascentor
  -> m Bool
menuGetShowPopup p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Menu* ptr)->GetShowPopup()} |]

-- | Return accelerator key code, 0 if disabled.
-- int GetAcceleratorKey() const { return acceleratorKey_; }
menuGetAcceleratorKey :: (Parent Menu a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Menu or ascentor
  -> m Int
menuGetAcceleratorKey p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int {$(Menu* ptr)->GetAcceleratorKey()} |]

-- | Return accelerator qualifiers.
-- int GetAcceleratorQualifiers() const { return acceleratorQualifiers_; }
menuGetAcceleratorQualifiers :: (Parent Menu a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Menu or ascentor
  -> m Int
menuGetAcceleratorQualifiers p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int {$(Menu* ptr)->GetAcceleratorQualifiers()} |]
