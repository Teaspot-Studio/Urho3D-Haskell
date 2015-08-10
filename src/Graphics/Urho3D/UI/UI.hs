{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.UI(
    UI
  , uiContext
  , uiRoot
  , uiFocusElement
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.UI
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> uiCntx <> objectContext <> uiElementContext)
C.include "<Urho3D/UI/UI.h>"
C.using "namespace Urho3D"

uiContext :: C.Context 
uiContext = objectContext <> uiCntx

instance Parent Object UI where 
  castToParent ptr = [C.pure| Object* { (Object*)$(UI* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| UI* { (UI*)$(Object* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Subsystem UI where 
  getSubsystemImpl ptr = [C.exp| UI* { $(Object* ptr)->GetSubsystem<UI>() } |]

-- | Returns root UI element
uiRoot :: (Pointer p a, Parent UI a, MonadIO m) => p -> m (Ptr UIElement)
uiRoot ptr = liftIO $ do 
  let ptr' = parentPointer ptr 
  [C.exp| UIElement* { $(UI* ptr')->GetRoot() } |]

-- | Returns current element in focus
uiFocusElement :: (Pointer p a, Parent UI a, MonadIO m) => p -> m (Maybe (Ptr UIElement))
uiFocusElement ptr = liftIO $ do 
  let ptr' = parentPointer ptr 
  fe <- [C.exp| UIElement* { $(UI* ptr')->GetFocusElement() } |]
  checkNullPtr' fe return