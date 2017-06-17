{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.CheckBox(
    CheckBox
  , checkBoxContext
  , SharedCheckBox
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
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx <> sharedCheckBoxPtrCntx <> checkBoxCntx <> contextContext <> uiElementContext <> borderImageContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/UI/CheckBox.h>"
C.using "namespace Urho3D"

checkBoxContext :: C.Context
checkBoxContext = sharedCheckBoxPtrCntx <> checkBoxCntx

instance Creatable (Ptr CheckBox) where
  type CreationOptions (Ptr CheckBox) = Ptr Context

  newObject ptr = liftIO $ [C.exp| CheckBox* { new CheckBox( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(CheckBox* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''CheckBox

instance UIElem CheckBox where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { CheckBox::GetTypeStatic().Value() } |]

sharedPtr "CheckBox"
