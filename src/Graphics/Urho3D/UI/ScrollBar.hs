{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.ScrollBar(
    ScrollBar
  , scrollBarContext
  , SharedScrollBar
  , scrollBarSetOrientation
  , scrollBarSetRange
  , scrollBarSetValue
  , scrollBarChangeValue
  , scrollBarSetScrollStep
  , scrollBarSetStepFactor
  , scrollBarStepBack
  , scrollBarStepForward
  , scrollBarGetOrientation
  , scrollBarGetRange
  , scrollBarGetValue
  , scrollBarGetScrollStep
  , scrollBarGetStepFactor
  , scrollBarGetEffectiveScrollStep
  , scrollBarGetBackButton
  , scrollBarGetForwardButton
  , scrollBarGetSlider
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.ScrollBar
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
import Graphics.Urho3D.UI.Button
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.Slider

C.context (C.cppCtx
  <> sharedScrollBarPtrCntx
  <> scrollBarCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> objectContext
  <> animatableContext
  <> serializableContext
  <> vector2Context
  <> sliderContext
  <> buttonContext
  )
C.include "<Urho3D/UI/BorderImage.h>"
C.include "<Urho3D/UI/ScrollBar.h>"
C.using "namespace Urho3D"

scrollBarContext :: C.Context
scrollBarContext = sharedScrollBarPtrCntx
  <> scrollBarCntx

instance Creatable (Ptr ScrollBar) where
  type CreationOptions (Ptr ScrollBar) = Ptr Context

  newObject ptr = liftIO $ [C.exp| ScrollBar* { new ScrollBar( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(ScrollBar* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''ScrollBar

instance UIElem ScrollBar where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { ScrollBar::GetTypeStatic().Value() } |]

sharedPtr "ScrollBar"

-- | Set orientation type.
-- void SetOrientation(Orientation orientation);
scrollBarSetOrientation :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> Orientation
  -> m ()
scrollBarSetOrientation p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral . fromEnum $ v
  [C.exp| void {$(ScrollBar* ptr)->SetOrientation((Orientation)$(int v'))} |]

-- | Set slider range maximum value (minimum value is always 0.)
-- void SetRange(float range);
scrollBarSetRange :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> Float -- ^ range
  -> m ()
scrollBarSetRange p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollBar* ptr)->SetRange($(float v'))} |]

-- | Set slider current value.
-- void SetValue(float value);
scrollBarSetValue :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> Float -- ^ value
  -> m ()
scrollBarSetValue p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollBar* ptr)->SetValue($(float v'))} |]

-- | Change slider current value by a delta.
-- void ChangeValue(float delta);
scrollBarChangeValue :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> Float -- ^ delta
  -> m ()
scrollBarChangeValue p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollBar* ptr)->ChangeValue($(float v'))} |]

-- | Set button scroll step.
-- void SetScrollStep(float step);
scrollBarSetScrollStep :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> Float -- ^ step
  -> m ()
scrollBarSetScrollStep p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollBar* ptr)->SetScrollStep($(float v'))} |]

-- | Set button step factor, can be used to adjust the step for constant pixel size.
-- void SetStepFactor(float factor);
scrollBarSetStepFactor :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> Float -- ^ factor
  -> m ()
scrollBarSetStepFactor p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollBar* ptr)->SetStepFactor($(float v'))} |]

-- | Scroll back one step.
-- void StepBack();
scrollBarStepBack :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m ()
scrollBarStepBack p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(ScrollBar* ptr)->StepBack()} |]

-- | Scroll forward one step.
-- void StepForward();
scrollBarStepForward :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m ()
scrollBarStepForward p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(ScrollBar* ptr)->StepForward()} |]


-- | Return scrollbar orientation.
-- Orientation GetOrientation() const;
scrollBarGetOrientation :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m Orientation
scrollBarGetOrientation p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(ScrollBar* ptr)->GetOrientation()} |]

-- | Return slider range.
-- float GetRange() const;
scrollBarGetRange :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m Float
scrollBarGetRange p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollBar* ptr)->GetRange()} |]

-- | Return slider current value.
-- float GetValue() const;
scrollBarGetValue :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m Float
scrollBarGetValue p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollBar* ptr)->GetValue()} |]

-- | Return button scroll step.
-- float GetScrollStep() const { return scrollStep_; }
scrollBarGetScrollStep :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m Float
scrollBarGetScrollStep p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollBar* ptr)->GetScrollStep()} |]

-- | Return button step factor.
-- float GetStepFactor() const { return stepFactor_; }
scrollBarGetStepFactor :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m Float
scrollBarGetStepFactor p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollBar* ptr)->GetStepFactor()} |]

-- | Return scroll step multiplied by factor.
-- float GetEffectiveScrollStep() const;
scrollBarGetEffectiveScrollStep :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m Float
scrollBarGetEffectiveScrollStep p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollBar* ptr)->GetEffectiveScrollStep()} |]

-- | Return back button element.
-- Button* GetBackButton() const { return backButton_; }
scrollBarGetBackButton :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m (Ptr Button)
scrollBarGetBackButton p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Button* {$(ScrollBar* ptr)->GetBackButton()} |]

-- | Return forward button element.
-- Button* GetForwardButton() const { return forwardButton_; }
scrollBarGetForwardButton :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m (Ptr Button)
scrollBarGetForwardButton p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Button* {$(ScrollBar* ptr)->GetForwardButton()} |]

-- | Return slider element.
-- Slider* GetSlider() const { return slider_; }
scrollBarGetSlider :: (Parent ScrollBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollBar or ascentor
  -> m (Ptr Slider)
scrollBarGetSlider p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Slider* {$(ScrollBar* ptr)->GetSlider()} |]
