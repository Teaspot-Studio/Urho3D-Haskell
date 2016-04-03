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
 Static 3D scene example.
 This sample demonstrates:
     - Creating a 3D scene with static content
     - Displaying the scene using the Renderer subsystem
     - Handling keyboard and mouse input to move a freelook camera
-}
module Main where

import qualified Data.Text as T
import Control.Lens hiding (Context, element)
import Control.Monad 
import Data.IORef
import Data.Monoid
import Foreign
import Graphics.Urho3D
import Sample

main :: IO ()
main = withObject () $ \cntx -> do 
  newSample cntx "StaticScene" joysticPatch customStart >>= runSample

-- | Helper function that prints profided message when get Nothing
fromJustTrace :: String -> Maybe a -> a 
fromJustTrace msg Nothing = error $ "fromJust: " ++ msg 
fromJustTrace _ (Just a) = a 

-- | Setup after engine initialization and before running the main loop.
customStart :: SampleRef -> IO ()
customStart sr = do 
  s <- readIORef sr 
  let app = s ^. sampleApplication

  -- Create the scene content 
  (scene, cameraNode) <- createScene app 
  -- Create the UI content 
  createInstructions app 
  -- Setup the viewport for displaying the scene
  setupViewport app scene cameraNode
  -- Hook up to the frame update events 
  subscribeToEvents app cameraNode

-- | Construct the scene content.
createScene :: SharedApplicationPtr -> IO (SharedScenePtr, Ptr Node)
createScene app = do 
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app 

  (scene :: SharedScenePtr) <- newSharedObject =<< getContext app 

  {-
   Create the Octree component to the scene. This is required before adding any drawable components, or else nothing will
   show up. The default octree volume will be from (-1000, -1000, -1000) to (1000, 1000, 1000) in world coordinates; it
   is also legal to place objects outside the volume but their visibility can then not be checked in a hierarchically
   optimizing manner
  -}
  (_ :: Ptr Octree) <- fromJustTrace "Octree" <$> nodeCreateComponent scene Nothing Nothing

  {-
   Create a child scene node (at world origin) and a StaticModel component into it. Set the StaticModel to show a simple
   plane mesh with a "stone" material. Note that naming the scene nodes is optional. Scale the scene node larger
   (100 x 100 world units)
  -}
  planeNode <- nodeCreateChild scene "Plane" CM'Replicated 0
  nodeSetScale planeNode (Vector3 100 1 100)
  (planeObject :: Ptr StaticModel) <- fromJustTrace "Plane StaticModel" <$> nodeCreateComponent planeNode Nothing Nothing
  (planeModel :: Ptr Model) <- fromJustTrace "Plane.mdl" <$> cacheGetResource cache "Models/Plane.mdl" True 
  staticModelSetModel planeObject planeModel
  (planeMaterial :: Ptr Material) <- fromJustTrace "StoneTiled.xml" <$> cacheGetResource cache "Materials/StoneTiled.xml" True
  staticModelSetMaterial planeObject planeMaterial

  {-
   Create a directional light to the world so that we can see something. The light scene node's orientation controls the
   light direction; we will use the SetDirection() function which calculates the orientation from a forward direction vector.
   The light will use default settings (white light, no shadows)
  -}
  lightNode <- nodeCreateChild scene "DirectionalLight" CM'Replicated 0
  nodeSetDirection lightNode (Vector3 0.6 (-1.0) 0.8)
  (light :: Ptr Light) <- fromJustTrace "Light" <$> nodeCreateComponent lightNode Nothing Nothing
  lightSetLightType light LT'Directional

  {-
   Create more StaticModel objects to the scene, randomly positioned, rotated and scaled. For rotation, we construct a
   quaternion from Euler angles where the Y angle (rotation about the Y axis) is randomized. The mushroom model contains
   LOD levels, so the StaticModel component will automatically select the LOD level according to the view distance (you'll
   see the model get simpler as it moves further away). Finally, rendering a large number of the same object with the
   same material allows instancing to be used, if the GPU supports it. This reduces the amount of CPU work in rendering the
   scene.
  -}
  let numObjects = 200
  _ <- replicateM numObjects $ do 
    mushroomNode <- nodeCreateChild scene "Mushroom" CM'Replicated 0
    [r1, r2] <- replicateM 2 (randomUp 90)
    nodeSetPosition mushroomNode $ Vector3 (r1 - 45) 0 (r2 - 45)
    r3 <- randomUp 360 
    nodeSetRotation mushroomNode $ quaternionFromEuler 0 r3 0
    r4 <- randomUp 2
    nodeSetScale' mushroomNode $ 0.5 + r4

    (mushroomObject :: Ptr StaticModel) <- fromJustTrace "Mushroom StaticModel" <$> nodeCreateComponent mushroomNode Nothing Nothing
    (mushroomModel :: Ptr Model) <- fromJustTrace "Mushroom.mdl" <$> cacheGetResource cache "Models/Mushroom.mdl" True
    staticModelSetModel mushroomObject mushroomModel
    (mushroomMaterial :: Ptr Material) <- fromJustTrace "Mushroom.xml" <$> cacheGetResource cache "Materials/Mushroom.xml" True
    staticModelSetMaterial mushroomObject mushroomMaterial

  {-
    Create a scene node for the camera, which we will move around
    The camera will use default settings (1000 far clip distance, 45 degrees FOV, set aspect ratio automatically)
  -}
  cameraNode <- nodeCreateChild scene "Camera" CM'Replicated 0
  (_ :: Ptr Camera) <- fromJustTrace "Camera component" <$> nodeCreateComponent cameraNode Nothing Nothing

  -- Set an initial position for the camera scene node above the plane
  nodeSetPosition cameraNode $ Vector3 0 5.0 0

  return (scene, cameraNode)

-- | Construct an instruction text to the UI.
createInstructions :: SharedApplicationPtr -> IO ()
createInstructions app = do 
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app 
  (ui :: Ptr UI) <- fromJustTrace "UI" <$> getSubsystem app
  root <- uiRoot ui 

  -- Construct new Text object, set string to display and font to use
  (instructionText :: Ptr Text) <- createChildSimple root
  textSetText instructionText "Use WASD keys and mouse/touch to move"
  (font :: Ptr Font) <- fromJustTrace "Anonymous Pro.ttf" <$> cacheGetResource cache "Fonts/Anonymous Pro.ttf" True
  textSetFont instructionText font 15

  -- Position the text relative to the screen center
  uiElementSetAlignment instructionText AlignmentHorizontalCenter AlignmentVerticalCenter
  rootHeight <- uiElementGetHeight root 
  uiElementSetPosition instructionText $ IntVector2 0 (rootHeight `div` 4)

-- | Set up a viewport for displaying the scene.
setupViewport :: SharedApplicationPtr -> SharedScenePtr -> Ptr Node -> IO ()
setupViewport app scene cameraNode = do 
  (renderer :: Ptr Renderer) <- fromJustTrace "Renderer" <$> getSubsystem app

  {-
    Set up a viewport to the Renderer subsystem so that the 3D scene can be seen. We need to define the scene and the camera
    at minimum. Additionally we could configure the viewport screen size and the rendering path (eg. forward / deferred) to
    use, but now we just use full screen and default render path configured in the engine command line options
  -}
  cntx <- getContext app 
  (camera :: Ptr Camera) <- fromJustTrace "Camera" <$> nodeGetComponent cameraNode 
  (viewport :: SharedViewportPtr) <- newSharedObject (cntx, pointer scene, camera)
  rendererSetViewport renderer 0 viewport

data CameraData = CameraData {
  camYaw :: Float 
, camPitch :: Float
}

-- | Read input and moves the camera.
moveCamera :: SharedApplicationPtr -> Ptr Node -> Float -> CameraData -> IO CameraData
moveCamera app cameraNode timeStep camData = do 
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
    whenM (inputGetKeyDown input 'W') $ 
      nodeTranslate cameraNode (vec3Forward `mul` (moveSpeed * timeStep)) TS'Local
    whenM (inputGetKeyDown input 'S') $ 
      nodeTranslate cameraNode (vec3Back `mul` (moveSpeed * timeStep)) TS'Local
    whenM (inputGetKeyDown input 'A') $ 
      nodeTranslate cameraNode (vec3Left `mul` (moveSpeed * timeStep)) TS'Local
    whenM (inputGetKeyDown input 'D') $ 
      nodeTranslate cameraNode (vec3Right `mul` (moveSpeed * timeStep)) TS'Local

    return camData {
        camYaw = yaw 
      , camPitch = pitch
      }
  where 
    mul (Vector3 a b c) v = Vector3 (a*v) (b*v) (c*v)

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SharedApplicationPtr -> Ptr Node -> IO ()
subscribeToEvents app cameraNode = do 
  camDataRef <- newIORef $ CameraData 0 0
  subscribeToEvent app $ handleUpdate app cameraNode camDataRef

-- | Handle the logic update event.
handleUpdate :: SharedApplicationPtr -> Ptr Node -> IORef CameraData -> EventUpdate -> IO ()
handleUpdate app cameraNode camDataRef e = do 
  -- Take the frame time step, which is stored as a float
  let t = e ^. timeStep
  camData <- readIORef camDataRef
  -- Move the camera, scale movement with time step
  writeIORef camDataRef =<< moveCamera app cameraNode t camData

-- | Helper to run code when value is nothing
whenNothing :: Monad m => Maybe a -> b -> m b -> m b
whenNothing Nothing _ f = f 
whenNothing (Just _) a _ = return a