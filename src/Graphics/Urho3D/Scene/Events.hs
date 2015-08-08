{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Urho3D.Scene.Events(
    EventSceneUpdate(..)
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Scene.Scene
import Data.Monoid
import Data.Maybe 
import Foreign 

C.context (C.cppCtx <> stringHashContext)
C.include "<Urho3D/Scene/SceneEvents.h>"
C.using "namespace Urho3D"

-- | Fires within timestep for updating scene
data EventSceneUpdate = EventSceneUpdate

instance Event EventSceneUpdate where 
  data EventData EventSceneUpdate = EventSceneUpdateData {
    sceneUpdateScene :: Ptr Scene
  , sceneUpdateTimestep :: Float
  }

  eventID _ = [C.pure| const StringHash* {&E_SCENEUPDATE} |]
  loadEventData vmap = do 
    pscene <- variantMapGet' vmap [C.pure| const StringHash* {&SceneUpdate::P_SCENE} |]
    pt <- variantMapGet' vmap [C.pure| const StringHash* {&SceneUpdate::P_TIMESTEP} |]
    return $ EventSceneUpdateData {
      sceneUpdateScene = fromMaybe nullPtr pscene
    , sceneUpdateTimestep = fromMaybe 0 pt 
    }