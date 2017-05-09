{-
 Copyright (c) 2008-2015 the Urho3D project.

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
-}
{-
 Animating 3D scene example.
 This sample demonstrates:
     - Creating a 3D scene and using a custom component to animate the objects
     - Controlling scene ambience with the Zone component
     - Attaching a light to an object (the camera)
-}
module Main where

import Control.Lens hiding (Context, element)
import Control.Monad
import Data.IORef
import Foreign
import Graphics.Urho3D
import Sample
import Rotator

main :: IO ()
main = withObject () $ \cntx -> do
  newSample cntx "AnimatingScene" joysticPatch (customStart cntx) >>= runSample

-- | Setup after engine initialization and before running the main loop.
customStart :: Ptr Context -> SampleRef -> IO ()
customStart cntx sr = do
  s <- readIORef sr
  let app = s ^. sampleApplication

  -- Register an object factory for our custom Rotator component so that we can create them to scene nodes
  rotatorType <- registerRotator cntx

  -- Create the scene content
  (scene, cameraNode) <- createScene app rotatorType
  -- Create the UI content
  createInstructions app
  -- Setup the viewport for displaying the scene
  setupViewport app scene cameraNode
  -- Hook up to the frame update events
  subscribeToEvents app cameraNode
  -- Save scene to prevent garbage collecting
  writeIORef sr $ sampleScene .~ scene $ s

-- | Construct the scene content.
createScene :: SharedPtr Application -> RotatorType -> IO (SharedPtr Scene, Ptr Node)
createScene app rotatorType = do
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app
  (scene :: SharedPtr Scene) <- newSharedObject =<< getContext app

  {-
   Create the Octree component to the scene. This is required before adding any drawable components, or else nothing will
   show up. The default octree volume will be from (-1000, -1000, -1000) to (1000, 1000, 1000) in world coordinates; it
   is also legal to place objects outside the volume but their visibility can then not be checked in a hierarchically
   optimizing manner
  -}
  (_ :: Ptr Octree) <- fromJustTrace "Octree" <$> nodeCreateComponent scene Nothing Nothing

  -- Create a Zone component into a child scene node. The Zone controls ambient lighting and fog settings. Like the Octree,
  -- it also defines its volume with a bounding box, but can be rotated (so it does not need to be aligned to the world X, Y
  -- and Z axes.) Drawable objects "pick up" the zone they belong to and use it when rendering; several zones can exist
  zoneNode <- nodeCreateChild scene "Zone" CM'Replicated 0
  (zone :: Ptr Zone) <- fromJustTrace "Zone" <$> nodeCreateComponent zoneNode Nothing Nothing
  -- Set same volume as the Octree, set a close bluish fog and some ambient light
  zoneSetBoundingBox zone $ BoundingBox (-1000) 1000
  zoneSetAmbientColor zone $ rgb 0.05 0.1 0.15
  zoneSetFogColor zone $ rgb 0.1 0.2 0.3
  zoneSetFogStart zone 10
  zoneSetFogEnd zone 100

  -- Create randomly positioned and oriented box StaticModels in the scene
  let numObjects = 2000
  _ <- replicateM numObjects $ do
    boxNode <- nodeCreateChild scene "Box" CM'Replicated 0
    [r1, r2, r3] <- replicateM 3 (randomUp 200)
    nodeSetPosition boxNode $ Vector3 (r1 - 100) (r2 - 100) (r3 - 100)
    -- Orient using random pitch, yaw and roll Euler angles
    [r4, r5, r6] <- replicateM 3 (randomUp 360)
    nodeSetRotation boxNode $ quaternionFromEuler r4 r5 r6

    (boxObject :: Ptr StaticModel) <- fromJustTrace "Box StaticModel" <$> nodeCreateComponent boxNode Nothing Nothing
    (boxModel :: Ptr Model) <- fromJustTrace "Box.mdl" <$> cacheGetResource cache "Models/Box.mdl" True
    staticModelSetModel boxObject boxModel
    (boxMaterial :: Ptr Material) <- fromJustTrace "Stone.xml" <$> cacheGetResource cache "Materials/Stone.xml" True
    staticModelSetMaterial boxObject boxMaterial

    -- Add our custom Rotator component which will rotate the scene node each frame, when the scene sends its update event.
    -- The Rotator component derives from the base class LogicComponent, which has convenience functionality to subscribe
    -- to the various update events, and forward them to virtual functions that can be implemented by subclasses. This way
    -- writing logic/update components in C++ becomes similar to scripting.
    -- Now we simply set same rotation speed for all objects
    (rotator :: Ptr Rotator) <- fromJustTrace "Rotator" <$> nodeCreateCustomComponent boxNode rotatorType Nothing Nothing
    setRotatorSpeed rotator $ Vector3 10 20 30
    return ()

  -- Create the camera. Let the starting position be at the world origin. As the fog limits maximum visible distance, we can
  -- bring the far clip plane closer for more effective culling of distant objects
  cameraNode <- nodeCreateChild scene "Camera" CM'Replicated 0
  (cam :: Ptr Camera) <- fromJustTrace "Camera component" <$> nodeCreateComponent cameraNode Nothing Nothing
  cameraSetFarClip cam 100

  -- Create a point light to the camera scene node
  (light :: Ptr Light) <- fromJustTrace "Light" <$> nodeCreateComponent cameraNode Nothing Nothing
  lightSetLightType light LT'Point
  lightSetRange light 30

  return (scene, cameraNode)

-- | Construct an instruction text to the UI.
createInstructions :: SharedPtr Application -> IO ()
createInstructions app = do
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app
  (ui :: Ptr UI) <- fromJustTrace "UI" <$> getSubsystem app
  roote <- uiRoot ui

  -- Construct new Text object, set string to display and font to use
  (instructionText :: Ptr Text) <- createChildSimple roote
  textSetText instructionText "Use WASD keys and mouse/touch to move"
  (font :: Ptr Font) <- fromJustTrace "Anonymous Pro.ttf" <$> cacheGetResource cache "Fonts/Anonymous Pro.ttf" True
  textSetFont instructionText font 15

  -- Position the text relative to the screen center
  uiElementSetAlignment instructionText AlignmentHorizontalCenter AlignmentVerticalCenter
  rootHeight <- uiElementGetHeight roote
  uiElementSetPosition instructionText $ IntVector2 0 (rootHeight `div` 4)

-- | Set up a viewport for displaying the scene.
setupViewport :: SharedPtr Application -> SharedPtr Scene -> Ptr Node -> IO ()
setupViewport app scene cameraNode = do
  (renderer :: Ptr Renderer) <- fromJustTrace "Renderer" <$> getSubsystem app

  {-
    Set up a viewport to the Renderer subsystem so that the 3D scene can be seen. We need to define the scene and the camera
    at minimum. Additionally we could configure the viewport screen size and the rendering path (eg. forward / deferred) to
    use, but now we just use full screen and default render path configured in the engine command line options
  -}
  cntx <- getContext app
  (cam :: Ptr Camera) <- fromJustTrace "Camera" <$> nodeGetComponent cameraNode False
  (viewport :: SharedPtr Viewport) <- newSharedObject (cntx, pointer scene, cam)
  rendererSetViewport renderer 0 viewport

data CameraData = CameraData {
  camYaw :: Float
, camPitch :: Float
}

-- | Read input and moves the camera.
moveCamera :: SharedPtr Application -> Ptr Node -> Float -> CameraData -> IO CameraData
moveCamera app cameraNode t camData = do
  (ui :: Ptr UI) <- fromJustTrace "UI" <$> getSubsystem app

  -- Do not move if the UI has a focused element (the console)
  mFocusElem <- uiFocusElement ui
  whenNothing mFocusElem camData $ do
    (input :: Ptr Input) <- fromJustTrace "Input" <$> getSubsystem app

    -- Movement speed as world units per second
    let moveSpeed = 20
    -- Mouse sensitivity as degrees per pixel
    let mouseSensitivity = 0.1

    -- Use this frame's mouse motion to adjust camera node yaw and pitch. Clamp the pitch between -90 and 90 degrees
    mouseMove <- inputGetMouseMove input
    let yaw = camYaw camData + mouseSensitivity * fromIntegral (mouseMove ^. x)
    let pitch = camPitch camData + mouseSensitivity * fromIntegral (mouseMove ^. y)

    -- Construct new orientation for the camera scene node from yaw and pitch. Roll is fixed to zero
    nodeSetRotation cameraNode $ quaternionFromEuler pitch yaw 0

    -- Read WASD keys and move the camera scene node to the corresponding direction if they are pressed
    -- Use the Translate() function (default local space) to move relative to the node's orientation.
    whenM (inputGetKeyDown input KeyW) $
      nodeTranslate cameraNode (vec3Forward `mul` (moveSpeed * t)) TS'Local
    whenM (inputGetKeyDown input KeyS) $
      nodeTranslate cameraNode (vec3Back `mul` (moveSpeed * t)) TS'Local
    whenM (inputGetKeyDown input KeyA) $
      nodeTranslate cameraNode (vec3Left `mul` (moveSpeed * t)) TS'Local
    whenM (inputGetKeyDown input KeyD) $
      nodeTranslate cameraNode (vec3Right `mul` (moveSpeed * t)) TS'Local

    return camData {
        camYaw = yaw
      , camPitch = pitch
      }
  where
    mul (Vector3 a b c) v = Vector3 (a*v) (b*v) (c*v)

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SharedPtr Application -> Ptr Node -> IO ()
subscribeToEvents app cameraNode = do
  camDataRef <- newIORef $ CameraData 0 0
  subscribeToEvent app $ handleUpdate app cameraNode camDataRef

-- | Handle the logic update event.
handleUpdate :: SharedPtr Application -> Ptr Node -> IORef CameraData -> EventUpdate -> IO ()
handleUpdate app cameraNode camDataRef e = do
  -- Take the frame time step, which is stored as a float
  let t = e ^. timeStep
  camData <- readIORef camDataRef
  -- Move the camera, scale movement with time step
  writeIORef camDataRef =<< moveCamera app cameraNode t camData
