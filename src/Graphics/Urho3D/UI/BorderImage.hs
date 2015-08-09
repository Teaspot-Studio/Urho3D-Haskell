{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.BorderImage(
    BorderImage 
  , borderImageContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.BorderImage
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> borderImageCntx <> contextContext <> uiElementContext)
C.include "<Urho3D/UI/BorderImage.h>"
C.using "namespace Urho3D"

borderImageContext :: C.Context 
borderImageContext = borderImageCntx

newBorderImage :: Ptr Context -> IO (Ptr BorderImage)
newBorderImage ptr = [C.exp| BorderImage* { new BorderImage( $(Context* ptr) ) } |]

deleteBorderImage :: Ptr BorderImage -> IO ()
deleteBorderImage ptr = [C.exp| void { delete $(BorderImage* ptr) } |]

instance Createable (Ptr BorderImage) where 
  type CreationOptions (Ptr BorderImage) = Ptr Context 

  newObject = liftIO . newBorderImage
  deleteObject = liftIO . deleteBorderImage

instance Parent UIElement BorderImage  where 
  castToParent ptr = [C.pure| UIElement* {(UIElement*)$(BorderImage* ptr)} |]
  castToChild ptr = let
    child = [C.pure| BorderImage* {(BorderImage*)$(UIElement* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance UIElem BorderImage where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = BorderImage::GetTypeStatic();  
      return &h;
    } |]