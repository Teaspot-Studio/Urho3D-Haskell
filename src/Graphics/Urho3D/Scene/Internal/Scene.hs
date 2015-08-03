{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Scene.Internal.Scene(
    Scene
  , sceneCntx
  , sharedScenePtrCntx
  , SharedScene 
  , SharedScenePtr
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Scene

sceneCntx :: C.Context 
sceneCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Scene", [t| Scene |])
    ]
  }

sharedPtrImpl "Scene"