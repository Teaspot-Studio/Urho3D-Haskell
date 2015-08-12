{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.CheckBox(
    CheckBox 
  , checkBoxContext
  , SharedCheckBox
  , SharedCheckBoxPtr 
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.CheckBox
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> sharedCheckBoxPtrCntx <> checkBoxCntx <> contextContext <> uiElementContext <> borderImageContext)
C.include "<Urho3D/UI/CheckBox.h>"
C.using "namespace Urho3D"

checkBoxContext :: C.Context 
checkBoxContext = sharedCheckBoxPtrCntx <> checkBoxCntx

instance Createable (Ptr CheckBox) where 
  type CreationOptions (Ptr CheckBox) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| CheckBox* { new CheckBox( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(CheckBox* ptr) } |]

instance Parent UIElement CheckBox  where 
  castToParent ptr = [C.pure| UIElement* {(UIElement*)$(CheckBox* ptr)} |]
  castToChild ptr = let
    child = [C.pure| CheckBox* {(CheckBox*)$(UIElement* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent BorderImage CheckBox  where 
  castToParent ptr = [C.pure| BorderImage* {(BorderImage*)$(CheckBox* ptr)} |]
  castToChild ptr = let
    child = [C.pure| CheckBox* {(CheckBox*)$(BorderImage* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance UIElem CheckBox where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = CheckBox::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "CheckBox"