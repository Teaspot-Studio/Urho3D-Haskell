{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Slider(
    Slider
  , sliderContext
  , SharedSlider
  , sliderSetOrientation
  , sliderSetRange
  , sliderSetValue
  , sliderChangeValue
  , sliderSetRepeatRate
  , sliderGetOrientation
  , sliderGetRange
  , sliderGetValue
  , sliderGetKnob
  , sliderGetRepeatRate
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Slider
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
  <> sharedSliderPtrCntx
  <> sliderCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> objectContext
  <> animatableContext
  <> serializableContext
  <> vector2Context
  )
C.include "<Urho3D/UI/Slider.h>"
C.using "namespace Urho3D"

sliderContext :: C.Context
sliderContext = sharedSliderPtrCntx
  <> sliderCntx

instance Creatable (Ptr Slider) where
  type CreationOptions (Ptr Slider) = Ptr Context

  newObject ptr = liftIO $ [C.exp| Slider* { new Slider( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Slider* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Slider

instance UIElem Slider where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Slider::GetTypeStatic().Value() } |]

sharedPtr "Slider"

-- | Set orientation type.
-- void SetOrientation(Orientation orientation);
sliderSetOrientation :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> Orientation
  -> m ()
sliderSetOrientation p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral . fromEnum $ v
  [C.exp| void {$(Slider* ptr)->SetOrientation((Orientation)$(int v'))} |]

-- | Set slider range maximum value (minimum value is always 0.)
-- void SetRange(float range);
sliderSetRange :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> Float -- ^ range
  -> m ()
sliderSetRange p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Slider* ptr)->SetRange($(float v'))} |]

-- | Set slider current value.
-- void SetValue(float value);
sliderSetValue :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> Float -- ^ value
  -> m ()
sliderSetValue p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Slider* ptr)->SetValue($(float v'))} |]

-- | Change value by a delta.
-- void ChangeValue(float delta);
sliderChangeValue :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> Float -- ^ delta
  -> m ()
sliderChangeValue p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Slider* ptr)->ChangeValue($(float v'))} |]

-- | Set paging minimum repeat rate (number of events per second).
-- void SetRepeatRate(float rate);
sliderSetRepeatRate :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> Float -- ^ rate
  -> m ()
sliderSetRepeatRate p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Slider* ptr)->SetRepeatRate($(float v'))} |]

-- | Return orientation type.
-- Orientation GetOrientation() const { return orientation_; }
sliderGetOrientation :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> m Orientation
sliderGetOrientation p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(Slider* ptr)->GetOrientation()} |]

-- | Return slider range.
-- float GetRange() const { return range_; }
sliderGetRange :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> m Float
sliderGetRange p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Slider* ptr)->GetRange()} |]

-- | Return slider current value.
-- float GetValue() const { return value_; }
sliderGetValue :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> m Float
sliderGetValue p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Slider* ptr)->GetValue()} |]

-- | Return knob element.
-- BorderImage* GetKnob() const { return knob_; }
sliderGetKnob :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> m (Ptr BorderImage)
sliderGetKnob p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| BorderImage* {$(Slider* ptr)->GetKnob()} |]

-- | Return paging minimum repeat rate (number of events per second).
-- float GetRepeatRate() const { return repeatRate_; }
sliderGetRepeatRate :: (Parent Slider a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to slider or ascentor
  -> m Float
sliderGetRepeatRate p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Slider* ptr)->GetRepeatRate()} |]
