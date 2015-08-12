{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Font(
    Font 
  , fontContext
  , SharedFont
  , SharedFontPtr
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Font
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> fontCntx <> contextContext <> resourceContext <> sharedFontPtrCntx)
C.include "<Urho3D/UI/Font.h>"
C.using "namespace Urho3D"

fontContext :: C.Context 
fontContext = fontCntx <> resourceContext <> sharedFontPtrCntx

instance Createable (Ptr Font) where 
  type CreationOptions (Ptr Font) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| Font* { new Font( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Font* ptr) } |]

instance ResourceType Font where 
  resourceType _ = unsafePerformIO $ [C.block| StringHash* { 
    static StringHash h = Font::GetTypeStatic(); 
    return &h; 
    } |]

sharedPtr "Font"