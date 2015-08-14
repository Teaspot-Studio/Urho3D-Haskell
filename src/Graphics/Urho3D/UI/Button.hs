{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Button(
    Button 
  , buttonContext
  , SharedButton
  , SharedButtonPtr 
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Button
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> sharedButtonPtrCntx <> buttonCntx <> contextContext <> uiElementContext <> borderImageContext <> objectContext)
C.include "<Urho3D/UI/Button.h>"
C.using "namespace Urho3D"

buttonContext :: C.Context 
buttonContext = sharedButtonPtrCntx <> buttonCntx

instance Createable (Ptr Button) where 
  type CreationOptions (Ptr Button) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| Button* { new Button( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Button* ptr) } |]

instance Parent UIElement Button  where 
  castToParent ptr = [C.pure| UIElement* {(UIElement*)$(Button* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Button* {(Button*)$(UIElement* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent BorderImage Button  where 
  castToParent ptr = [C.pure| BorderImage* {(BorderImage*)$(Button* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Button* {(Button*)$(BorderImage* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Object Button  where 
  castToParent ptr = [C.pure| Object* {(Object*)$(Button* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Button* {(Button*)$(Object* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance UIElem Button where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = Button::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "Button"