{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.LineEdit(
    LineEdit 
  , lineEditContext
  , SharedLineEdit
  , SharedLineEditPtr 
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.LineEdit
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> sharedLineEditPtrCntx <> lineEditCntx <> contextContext <> uiElementContext <> borderImageContext)
C.include "<Urho3D/UI/LineEdit.h>"
C.using "namespace Urho3D"

lineEditContext :: C.Context 
lineEditContext = sharedLineEditPtrCntx <> lineEditCntx

instance Createable (Ptr LineEdit) where 
  type CreationOptions (Ptr LineEdit) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| LineEdit* { new LineEdit( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(LineEdit* ptr) } |]

instance Parent UIElement LineEdit  where 
  castToParent ptr = [C.pure| UIElement* {(UIElement*)$(LineEdit* ptr)} |]
  castToChild ptr = let
    child = [C.pure| LineEdit* {(LineEdit*)$(UIElement* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent BorderImage LineEdit  where 
  castToParent ptr = [C.pure| BorderImage* {(BorderImage*)$(LineEdit* ptr)} |]
  castToChild ptr = let
    child = [C.pure| LineEdit* {(LineEdit*)$(BorderImage* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance UIElem LineEdit where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = LineEdit::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "LineEdit"