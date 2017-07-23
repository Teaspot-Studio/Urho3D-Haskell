{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Button(
    Button
  , buttonContext
  , SharedButton
  , buttonSetPressedOffset
  , buttonSetPressedChildOffset
  , buttonSetRepeat
  , buttonSetRepeatDelay
  , buttonSetRepeatRate
  , buttonGetPressedOffset
  , buttonGetPressedChildOffset
  , buttonGetRepeatDelay
  , buttonGetRepeatRate
  , buttonIsPressed
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Button
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
  <> sharedButtonPtrCntx
  <> buttonCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> objectContext
  <> animatableContext
  <> serializableContext
  <> vector2Context
  )
C.include "<Urho3D/UI/Button.h>"
C.using "namespace Urho3D"

buttonContext :: C.Context
buttonContext = sharedButtonPtrCntx
  <> buttonCntx

instance Creatable (Ptr Button) where
  type CreationOptions (Ptr Button) = Ptr Context

  newObject ptr = liftIO $ [C.exp| Button* { new Button( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Button* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Button

instance UIElem Button where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Button::GetTypeStatic().Value() } |]

sharedPtr "Button"

-- | Set offset to image rectangle used when pressed.
-- void SetPressedOffset(const IntVector2& offset);
buttonSetPressedOffset :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> IntVector2 -- ^ offset
  -> m ()
buttonSetPressedOffset p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Button* ptr)->SetPressedOffset(*$(IntVector2* v'))} |]

-- | Set offset of child elements when pressed.
-- void SetPressedChildOffset(const IntVector2& offset);
buttonSetPressedChildOffset :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> IntVector2 -- ^ offset
  -> m ()
buttonSetPressedChildOffset p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Button* ptr)->SetPressedChildOffset(*$(IntVector2* v'))} |]


-- | Set repeat properties. Rate 0 (default) disables repeat.
-- void SetRepeat(float delay, float rate);
buttonSetRepeat :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> Float -- ^ delay
  -> Float -- ^ rate
  -> m ()
buttonSetRepeat p d r = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
      r' = realToFrac r
  [C.exp| void {$(Button* ptr)->SetRepeat($(float d'), $(float r'))} |]

-- | Set repeat delay.
-- void SetRepeatDelay(float delay);
buttonSetRepeatDelay :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> Float -- ^ delay
  -> m ()
buttonSetRepeatDelay p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void {$(Button* ptr)->SetRepeatDelay($(float d'))} |]

-- | Set repeat rate.
-- void SetRepeatRate(float rate);
buttonSetRepeatRate :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> Float -- ^ rate
  -> m ()
buttonSetRepeatRate p r = liftIO $ do
  let ptr = parentPointer p
      r' = realToFrac r
  [C.exp| void {$(Button* ptr)->SetRepeatRate($(float r'))} |]

-- | Return pressed image offset.
-- const IntVector2& GetPressedOffset() const { return pressedOffset_; }
buttonGetPressedOffset :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> m IntVector2
buttonGetPressedOffset p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* {&$(Button* ptr)->GetPressedOffset()} |]

-- | Return offset of child elements when pressed.
-- const IntVector2& GetPressedChildOffset() const { return pressedChildOffset_; }
buttonGetPressedChildOffset :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> m IntVector2
buttonGetPressedChildOffset p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* {&$(Button* ptr)->GetPressedChildOffset()} |]

-- | Return repeat delay.
-- float GetRepeatDelay() const { return repeatDelay_; }
buttonGetRepeatDelay :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> m Float
buttonGetRepeatDelay p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Button* ptr)->GetRepeatDelay()} |]

-- | Return repeat rate.
-- float GetRepeatRate() const { return repeatRate_; }
buttonGetRepeatRate :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> m Float
buttonGetRepeatRate p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Button* ptr)->GetRepeatRate()} |]

-- | Return whether is currently pressed.
-- bool IsPressed() const { return pressed_; }
buttonIsPressed :: (Parent Button a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to button or ascentor
  -> m Bool
buttonIsPressed p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Button* ptr)->IsPressed()} |]
