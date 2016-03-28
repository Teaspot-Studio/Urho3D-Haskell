{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Camera(
    Camera
  , cameraContext
  , cameraGetFov
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Camera
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Scene.Node 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> cameraCntx <> componentContext)
C.include "<Urho3D/Graphics/Camera.h>"
C.using "namespace Urho3D"

cameraContext :: C.Context 
cameraContext = componentContext <> cameraCntx

instance Parent Component Camera where 
  castToParent ptr = [C.pure| Component* { (Component*)$(Camera* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Camera* { (Camera*)$(Component* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance NodeComponent Camera where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = Camera::GetTypeStatic();
    return &h;
  } |]

-- | Return vertical field of view in degrees.
cameraGetFov :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetFov p = liftIO $ do 
  let ptr = parentPointer p 
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetFov()} |]