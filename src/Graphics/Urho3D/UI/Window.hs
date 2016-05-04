{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Window(
    Window 
  , windowContext
  , SharedWindow
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Window
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> sharedWindowPtrCntx <> windowCntx <> contextContext <> uiElementContext <> borderImageContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/UI/Window.h>"
C.using "namespace Urho3D"

windowContext :: C.Context 
windowContext = sharedWindowPtrCntx <> windowCntx

instance Createable (Ptr Window) where 
  type CreationOptions (Ptr Window) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| Window* { new Window( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Window* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Window

instance UIElem Window where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = Window::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "Window"