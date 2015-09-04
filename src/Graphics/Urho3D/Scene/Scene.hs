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
import Graphics.Urho3D.Scene.Node
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> sceneCntx <> sharedScenePtrCntx <> contextContext <> nodeContext)
C.include "<Urho3D/Scene/Scene.h>"
C.using "namespace Urho3D"

sceneContext :: C.Context 
sceneContext = sharedScenePtrCntx <> sceneCntx <> nodeContext

newScene :: Ptr Context -> IO (Ptr Scene)
newScene ptr = [C.exp| Scene* { new Scene($(Context* ptr)) } |]

deleteScene :: Ptr Scene -> IO ()
deleteScene ptr = [C.exp| void { delete $(Scene* ptr) } |]

instance Createable (Ptr Scene) where 
  type CreationOptions (Ptr Scene) = Ptr Context 

  newObject = liftIO . newScene
  deleteObject = liftIO . deleteScene

instance Parent Node Scene where
  castToParent ptr = [C.pure| Node* {(Node*)$(Scene* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Scene* {(Scene*)$(Node* ptr)} |]
    in if child == nullPtr then Nothing else Just child

sharedPtr "Scene"