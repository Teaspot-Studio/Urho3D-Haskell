{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.ProgressBar(
    ProgressBar
  , progressBarContext
  , SharedProgressBar
  , progressBarSetOrientation
  , progressBarSetRange
  , progressBarSetValue
  , progressBarChangeValue
  , progressBarGetOrientation
  , progressBarGetRange
  , progressBarGetValue
  , progressBarGetKnob
  , progressBarSetLoadingPercentStyle
  , progressBarGetLoadingPercentStyle
  , progressBarSetShowPercentText
  , progressBarGetShowPercentText
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.ProgressBar
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import Foreign.C.String
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
  <> sharedProgressBarPtrCntx
  <> progressBarCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> objectContext
  <> animatableContext
  <> serializableContext
  <> vector2Context
  )
C.include "<Urho3D/UI/ProgressBar.h>"
C.using "namespace Urho3D"

progressBarContext :: C.Context
progressBarContext = sharedProgressBarPtrCntx
  <> progressBarCntx

instance Creatable (Ptr ProgressBar) where
  type CreationOptions (Ptr ProgressBar) = Ptr Context

  newObject ptr = liftIO $ [C.exp| ProgressBar* { new ProgressBar( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(ProgressBar* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''ProgressBar

instance UIElem ProgressBar where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { ProgressBar::GetTypeStatic().Value() } |]

sharedPtr "ProgressBar"

-- | Set orientation type.
-- void SetOrientation(Orientation orientation);
progressBarSetOrientation :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> Orientation -- ^ orientation
  -> m ()
progressBarSetOrientation p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral . fromEnum $ v
  [C.exp| void {$(ProgressBar* ptr)->SetOrientation((Orientation)$(int v'))} |]

-- | Set ProgressBar range maximum value (minimum value is always 0.)
-- void SetRange(float range);
progressBarSetRange :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> Float
  -> m ()
progressBarSetRange p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ProgressBar* ptr)->SetRange($(float v'))} |]

-- | Set ProgressBar current value.
-- void SetValue(float value);
progressBarSetValue :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> Float -- ^ value
  -> m ()
progressBarSetValue p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ProgressBar* ptr)->SetValue($(float v'))} |]

-- | Change value by a delta.
-- void ChangeValue(float delta);
progressBarChangeValue :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> Float -- ^ delta
  -> m ()
progressBarChangeValue p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ProgressBar* ptr)->ChangeValue($(float v'))} |]

-- | Return orientation type.
-- Orientation GetOrientation() const { return orientation_; }
progressBarGetOrientation :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> m ()
progressBarGetOrientation p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(ProgressBar* ptr)->GetOrientation()} |]

-- | Return ProgressBar range.
-- float GetRange() const { return range_; }
progressBarGetRange :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> m Float
progressBarGetRange p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ProgressBar* ptr)->GetRange()} |]

-- | Return ProgressBar current value.
-- float GetValue() const { return value_; }
progressBarGetValue :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> m Float
progressBarGetValue p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ProgressBar* ptr)->GetValue()} |]

-- | Return knob element.
-- BorderImage *GetKnob() const { return knob_; }
progressBarGetKnob :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> m (Ptr BorderImage)
progressBarGetKnob p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| BorderImage* {$(ProgressBar* ptr)->GetKnob()} |]

-- | Sets the loading percent style.
-- void SetLoadingPercentStyle(const String &style) { loadingPercentStyle_ = style; }
progressBarSetLoadingPercentStyle :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> String -- ^ style
  -> m ()
progressBarSetLoadingPercentStyle p str = liftIO $ withCString str $ \str' -> do
  let ptr = parentPointer p
  [C.exp| void {$(ProgressBar* ptr)->SetLoadingPercentStyle(String($(const char* str')))} |]

-- | Returns the loading percent style.
-- const String& GetLoadingPercentStyle() const { return loadingPercentStyle_; }
progressBarGetLoadingPercentStyle :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> m String
progressBarGetLoadingPercentStyle p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* {$(ProgressBar* ptr)->GetLoadingPercentStyle().CString()} |]

-- | Sets the flag to display the percent text.
-- void SetShowPercentText(bool showPercentText);
progressBarSetShowPercentText :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> Bool -- ^ show percent text?
  -> m ()
progressBarSetShowPercentText p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(ProgressBar* ptr)->SetShowPercentText($(int v') != 0)} |]

-- | Returns the flag to display the percent text.
-- bool GetShowPercentText() const { return showPercentText_; }
progressBarGetShowPercentText :: (Parent ProgressBar a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to progressBar or ascentor
  -> m Bool
progressBarGetShowPercentText p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(ProgressBar* ptr)->GetShowPercentText()} |]
