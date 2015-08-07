{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Scene(
    Scene
  , sceneContext
  , SharedScene 
  , SharedScenePtr
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Scene.Internal.Scene
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Control.Monad.IO.Class
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> sceneCntx <> sharedScenePtrCntx <> contextContext)
C.include "<Urho3D/Scene/Scene.h>"
C.using "namespace Urho3D"

sceneContext :: C.Context 
sceneContext = sharedScenePtrCntx <> sceneCntx

newScene :: Ptr Context -> IO (Ptr Scene)
newScene ptr = [C.exp| Scene* { new Scene($(Context* ptr)) } |]

deleteScene :: Ptr Scene -> IO ()
deleteScene ptr = [C.exp| void { delete $(Scene* ptr) } |]

instance Createable Scene where 
  type CreationOptions Scene = Ptr Context 

  newObject = liftIO . newScene
  deleteObject = liftIO . deleteScene

sharedPtr "Scene"