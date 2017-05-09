{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Camera(
    Camera
  , cameraContext
  , cameraSetNearClip
  , cameraSetFarClip
  , cameraSetFov
  , cameraGetFov
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Camera
import Graphics.Urho3D.Scene.Node
import Graphics.Urho3D.Monad
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> cameraCntx <> componentContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/Graphics/Camera.h>"
C.using "namespace Urho3D"

cameraContext :: C.Context
cameraContext = componentContext <> cameraCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''Component] ''Camera

instance NodeComponent Camera where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Camera::GetTypeStatic().Value() } |]

-- | Return vertical field of view in degrees.
cameraGetFov :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetFov p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetFov()} |]

-- | Set near clip distance.
cameraSetNearClip :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ Value of near clip plain
  -> m ()
cameraSetNearClip p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetNearClip($(float v'))} |]

-- | Set far clip distance.
cameraSetFarClip :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ Value of far clip plain
  -> m ()
cameraSetFarClip p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetFarClip($(float v'))} |]

-- | Set vertical field of view in degrees.
cameraSetFov :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ fov value
  -> m ()
cameraSetFov p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetFov($(float v'))} |]
