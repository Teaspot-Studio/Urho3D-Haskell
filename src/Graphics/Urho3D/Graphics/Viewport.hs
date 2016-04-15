{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Viewport(
    Viewport 
  , viewportContext
  , SharedViewport
  , SharedViewportPtr 
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign

import Graphics.Urho3D.Graphics.Internal.Viewport

import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Graphics.Camera
import Graphics.Urho3D.Scene.Scene 

C.context (C.cppCtx <> sharedViewportPtrCntx <> viewportCntx <> contextContext <> sceneContext <> cameraContext <> objectContext)
C.include "<Urho3D/Graphics/Viewport.h>"
C.using "namespace Urho3D"

viewportContext :: C.Context 
viewportContext = sharedViewportPtrCntx <> viewportCntx <> objectContext

instance Createable (Ptr Viewport) where 
  type CreationOptions (Ptr Viewport) = (Ptr Context, Ptr Scene, Ptr Camera)

  newObject (cntxPtr, scenePtr, camPtr) = liftIO $ [C.exp| Viewport* { new Viewport( $(Context* cntxPtr), $(Scene* scenePtr), $(Camera* camPtr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Viewport* ptr) } |]

instance Parent Object Viewport  where 
  castToParent ptr = [C.pure| Object* {(Object*)$(Viewport* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Viewport* {(Viewport*)$(Object* ptr)} |]
    in if child == nullPtr then Nothing else Just child

sharedPtr "Viewport"