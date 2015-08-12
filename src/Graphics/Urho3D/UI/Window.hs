{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Window(
    Window 
  , windowContext
  , SharedWindow
  , SharedWindowPtr 
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Window
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> sharedWindowPtrCntx <> windowCntx <> contextContext <> uiElementContext <> borderImageContext)
C.include "<Urho3D/UI/Window.h>"
C.using "namespace Urho3D"

windowContext :: C.Context 
windowContext = sharedWindowPtrCntx <> windowCntx

instance Createable (Ptr Window) where 
  type CreationOptions (Ptr Window) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| Window* { new Window( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Window* ptr) } |]

instance Parent UIElement Window  where 
  castToParent ptr = [C.pure| UIElement* {(UIElement*)$(Window* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Window* {(Window*)$(UIElement* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent BorderImage Window  where 
  castToParent ptr = [C.pure| BorderImage* {(BorderImage*)$(Window* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Window* {(Window*)$(BorderImage* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance UIElem Window where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = Window::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "Window"