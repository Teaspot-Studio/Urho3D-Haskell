{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Graphics(
    Graphics
  , graphicsContext
  , graphicsSetWindowIcon
  , graphicsSetWindowTitle
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Graphics
import Graphics.Urho3D.Resource.Image
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.C.String 

C.context (C.cppCtx <> graphicsCntx <> objectContext <> imageContext)
C.include "<Urho3D/Graphics/Graphics.h>"
C.using "namespace Urho3D"

graphicsContext :: C.Context 
graphicsContext = objectContext <> graphicsCntx

instance Parent Object Graphics where 
  castToParent ptr = [C.pure| Object* { (Object*)$(Graphics* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Graphics* { (Graphics*)$(Object* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Subsystem Graphics where 
  getSubsystemImpl ptr = [C.exp| Graphics* { $(Object* ptr)->GetSubsystem<Graphics>() } |]

-- | Sets current window icon
graphicsSetWindowIcon :: (Pointer p Graphics, MonadIO m) => p -- ^ Pointer to graphics system
  -> Ptr Image -- ^ Pointer to icon resource
  -> m ()
graphicsSetWindowIcon ptr icon = liftIO $ do 
  let ptr' = pointer ptr 
  [C.exp| void { $(Graphics* ptr')->SetWindowIcon($(Image* icon)) }|]

-- | Sets current window title
graphicsSetWindowTitle :: (Pointer p Graphics, MonadIO m) => p -- ^ Pointer to graphics system
  -> String -- ^ Title
  -> m ()
graphicsSetWindowTitle ptr str = liftIO $ withCString str $ \str' -> do 
  let ptr' = pointer ptr 
  [C.exp| void { $(Graphics* ptr')->SetWindowTitle(String($(const char* str'))) }|]