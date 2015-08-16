{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.ToolTip(
    ToolTip 
  , toolTipContext
  , SharedToolTip
  , SharedToolTipPtr 
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.ToolTip
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> sharedToolTipPtrCntx <> toolTipCntx <> contextContext <> uiElementContext)
C.include "<Urho3D/UI/ToolTip.h>"
C.using "namespace Urho3D"

toolTipContext :: C.Context 
toolTipContext = sharedToolTipPtrCntx <> toolTipCntx

instance Createable (Ptr ToolTip) where 
  type CreationOptions (Ptr ToolTip) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| ToolTip* { new ToolTip( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(ToolTip* ptr) } |]

instance Parent UIElement ToolTip  where 
  castToParent ptr = [C.pure| UIElement* {(UIElement*)$(ToolTip* ptr)} |]
  castToChild ptr = let
    child = [C.pure| ToolTip* {(ToolTip*)$(UIElement* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance UIElem ToolTip where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = ToolTip::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "ToolTip"