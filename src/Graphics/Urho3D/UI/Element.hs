{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Element(
    UIElement 
  , uiElementContext
  , SharedUIElement
  , SharedUIElementPtr 
  , uiElementSetVisible
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Element
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid

C.context (C.cppCtx <> sharedUIElementPtrCntx <> uiElementCntx)
C.include "<Urho3D/UI/UIElement.h>"
C.using "namespace Urho3D"

uiElementContext :: C.Context 
uiElementContext = sharedUIElementPtrCntx <> uiElementCntx

sharedPtr "UIElement" 

-- | Hides or shows UI element
uiElementSetVisible :: (Pointer p UIElement, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Flag of visibility, True - visible, False - hided
  -> m ()
uiElementSetVisible ptr flag = liftIO $ do 
  let ptr' = pointer ptr 
      flag' = if flag then 1 else 0
  [C.exp| void { $(UIElement* ptr')->SetVisible($(int flag') != 0) } |]