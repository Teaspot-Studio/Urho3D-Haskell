{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.CheckBox(
    CheckBox
  , checkBoxContext
  , SharedCheckBox
  , checkBoxSetChecked
  , checkBoxSetCheckedOffset
  , checkBoxIsChecked
  , checkBoxGetCheckedOffset
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.CheckBox
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
  <> sharedCheckBoxPtrCntx
  <> checkBoxCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> vector2Context)
C.include "<Urho3D/UI/CheckBox.h>"
C.using "namespace Urho3D"

checkBoxContext :: C.Context
checkBoxContext = sharedCheckBoxPtrCntx
  <> checkBoxCntx

instance Creatable (Ptr CheckBox) where
  type CreationOptions (Ptr CheckBox) = Ptr Context

  newObject ptr = liftIO $ [C.exp| CheckBox* { new CheckBox( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(CheckBox* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''CheckBox

instance UIElem CheckBox where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { CheckBox::GetTypeStatic().Value() } |]

sharedPtr "CheckBox"

-- | Set checked state.
-- void SetChecked(bool enable);
checkBoxSetChecked :: (Parent CheckBox a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to CheckBox or ascentor
  -> Bool -- ^ enable
  -> m ()
checkBoxSetChecked p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(CheckBox* ptr)->SetChecked($(int v') != 0)} |]

-- | Set checked image offset.
-- void SetCheckedOffset(const IntVector2& rect);
checkBoxSetCheckedOffset :: (Parent CheckBox a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to CheckBox or ascentor
  -> IntVector2 -- ^ rect
  -> m ()
checkBoxSetCheckedOffset p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(CheckBox* ptr)->SetCheckedOffset(*$(IntVector2* v'))} |]

-- | Return whether is checked.
-- bool IsChecked() const { return checked_; }
checkBoxIsChecked :: (Parent CheckBox a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to CheckBox or ascentor
  -> m Bool
checkBoxIsChecked p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(CheckBox* ptr)->IsChecked()} |]

-- | Return checked image offset.
-- const IntVector2& GetCheckedOffset() const { return checkedOffset_; }
checkBoxGetCheckedOffset :: (Parent CheckBox a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to CheckBox or ascentor
  -> m IntVector2
checkBoxGetCheckedOffset p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* {&$(CheckBox* ptr)->GetCheckedOffset()} |]
