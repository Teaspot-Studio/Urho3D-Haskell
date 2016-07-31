{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Scene(
    Scene
  , sceneContext
  , SharedScene
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Scene.Internal.Scene
import Graphics.Urho3D.Scene.Node
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Data.Monoid
import Foreign 

import Graphics.Urho3D.Scene.Animatable 
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Core.Object 

C.context (C.cppCtx <> sceneCntx <> sharedScenePtrCntx <> contextContext <> nodeContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/Scene/Scene.h>"
C.using "namespace Urho3D"

sceneContext :: C.Context 
sceneContext = sharedScenePtrCntx <> sceneCntx <> nodeContext

newScene :: Ptr Context -> IO (Ptr Scene)
newScene ptr = [C.exp| Scene* { new Scene($(Context* ptr)) } |]

deleteScene :: Ptr Scene -> IO ()
deleteScene ptr = [C.exp| void { delete $(Scene* ptr) } |]

instance Creatable (Ptr Scene) where 
  type CreationOptions (Ptr Scene) = Ptr Context 

  newObject = liftIO . newScene
  deleteObject = liftIO . deleteScene

deriveParents [''Object, ''Serializable, ''Animatable, ''Node] ''Scene

sharedPtr "Scene"