{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.LogicComponent(
    LogicComponent
  , logicComponentContext
  , SharedLogicComponent
  , SharedLogicComponentPtr
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Scene.Internal.LogicComponent
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

import Graphics.Urho3D.Scene.Animatable 
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx <> logicComponentCntx <> sharedLogicComponentPtrCntx <> contextContext <> stringHashContext <> animatableContext <> componentContext <> serializableContext)
C.include "<Urho3D/Scene/LogicComponent.h>"
C.using "namespace Urho3D" 

logicComponentContext :: C.Context 
logicComponentContext = sharedLogicComponentPtrCntx <> logicComponentCntx <> stringHashContext

newLogicComponent :: Ptr Context -> IO (Ptr LogicComponent)
newLogicComponent ptr = [C.exp| LogicComponent* { new LogicComponent($(Context* ptr)) } |]

deleteLogicComponent :: Ptr LogicComponent -> IO ()
deleteLogicComponent ptr = [C.exp| void { delete $(LogicComponent* ptr) } |]

instance Createable (Ptr LogicComponent) where 
  type CreationOptions (Ptr LogicComponent) = Ptr Context 

  newObject = liftIO . newLogicComponent
  deleteObject = liftIO . deleteLogicComponent

sharedPtr "LogicComponent" 

instance Parent Component LogicComponent where
  castToParent ptr = [C.pure| Component* {(Component*)$(LogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| LogicComponent* {(LogicComponent*)$(Component* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Animatable LogicComponent where
  castToParent ptr = [C.pure| Animatable* {(Animatable*)$(LogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| LogicComponent* {(LogicComponent*)$(Animatable* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Serializable LogicComponent where
  castToParent ptr = [C.pure| Serializable* {(Serializable*)$(LogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| LogicComponent* {(LogicComponent*)$(Serializable* ptr)} |]
    in if child == nullPtr then Nothing else Just child