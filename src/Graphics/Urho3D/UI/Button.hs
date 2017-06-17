{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Button(
    Button
  , buttonContext
  , SharedButton
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
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx <> sharedButtonPtrCntx <> buttonCntx <> contextContext <> uiElementContext <> borderImageContext <> objectContext <> animatableContext <> serializableContext)
C.include "<Urho3D/UI/Button.h>"
C.using "namespace Urho3D"

buttonContext :: C.Context
buttonContext = sharedButtonPtrCntx <> buttonCntx

instance Creatable (Ptr Button) where
  type CreationOptions (Ptr Button) = Ptr Context

  newObject ptr = liftIO $ [C.exp| Button* { new Button( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Button* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Button

instance UIElem Button where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Button::GetTypeStatic().Value() } |]

sharedPtr "Button"
