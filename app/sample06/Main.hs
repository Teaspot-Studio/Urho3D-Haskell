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
  Skeletal animation example.
  This sample demonstrates:
      - Populating a 3D scene with skeletally animated AnimatedModel components;
      - Moving the animated models and advancing their animation using a custom component
      - Enabling a cascaded shadow map on a directional light, which allows high-quality shadows
        over a large area (typically used in outdoor scenes for shadows cast by sunlight)
      - Displaying renderer debug geometry
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
import Mover 

main :: IO ()
main = withObject () $ \cntx -> do 
  newSample cntx "SkeletalAnimation" joysticPatch (customStart cntx) >>= runSample

-- | Setup after engine initialization and before running the main loop.
customStart :: Ptr Context -> SampleRef -> IO ()
customStart cntx sr = do 
  s <- readIORef sr 
  let app = s ^. sampleApplication
  
  -- Register an object factory for our custom Mover component so that we can create them to scene nodes
  moverType <- registerMover cntx

  -- Create the scene content 
  (scene, cameraNode) <- createScene app moverType
  -- Create the UI content 
  createInstructions app 
  -- Setup the viewport for displaying the scene
  setupViewport app scene cameraNode
  -- Hook up to the frame update events 
  subscribeToEvents app cameraNode
  -- Save scene to prevent garbage collecting 
  writeIORef sr $ sampleScene .~ scene $ s 

-- | Construct the scene content.
createScene :: SharedApplicationPtr -> MoverType -> IO (SharedScenePtr, Ptr Node)
createScene app moverType = do 
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app 
  (scene :: SharedScenePtr) <- newSharedObject =<< getContext app 

  {-
    Create octree, use default volume (-1000, -1000, -1000) to (1000, 1000, 1000)
    Also create a DebugRenderer component so that we can draw debug geometry
  -}
  (_ :: Ptr Octree) <- fromJustTrace "Octree" <$> nodeCreateComponent scene Nothing Nothing
  (_ :: Ptr DebugRenderer) <- fromJustTrace "DebugRenderer" <$> nodeCreateComponent scene Nothing Nothing

  -- Create scene node & StaticModel component for showing a static plane
  planeNode <- nodeCreateChild scene "Plane" CM'Replicated 0
  nodeSetScale planeNode (Vector3 100 1 100)
  (planeObject :: Ptr StaticModel) <- fromJustTrace "Plane StaticModel" <$> nodeCreateComponent planeNode Nothing Nothing
  (planeModel :: Ptr Model) <- fromJustTrace "Plane.mdl" <$> cacheGetResource cache "Models/Plane.mdl" True 
  staticModelSetModel planeObject planeModel
  (planeMaterial :: Ptr Material) <- fromJustTrace "StoneTiled.xml" <$> cacheGetResource cache "Materials/StoneTiled.xml" True
  staticModelSetMaterial planeObject planeMaterial

  -- Create a Zone component for ambient lighting & fog control
  zoneNode <- nodeCreateChild scene "Zone" CM'Replicated 0
  (zone :: Ptr Zone) <- fromJustTrace "Zone" <$> nodeCreateComponent zoneNode Nothing Nothing
  -- Set same volume as the Octree, set a close bluish fog and some ambient light
  zoneSetBoundingBox zone $ BoundingBox (-1000) 1000
  zoneSetAmbientColor zone $ rgb 0.15 0.15 0.15
  zoneSetFogColor zone $ rgb 0.5 0.5 0.7
  zoneSetFogStart zone 100
  zoneSetFogEnd zone 300

  {-
   Create a directional light to the world so that we can see something. The light scene node's orientation controls the
   light direction; we will use the SetDirection() function which calculates the orientation from a forward direction vector.
   The light will use default settings (white light, no shadows)
  -}
  lightNode <- nodeCreateChild scene "DirectionalLight" CM'Replicated 0
  nodeSetDirection lightNode (Vector3 0.6 (-1.0) 0.8)
  (light :: Ptr Light) <- fromJustTrace "Light" <$> nodeCreateComponent lightNode Nothing Nothing
  lightSetLightType light LT'Directional
  drawableSetCastShadows light True 
  lightSetShadowBias light $ BiasParameters 0.00025 0.5
  -- Set cascade splits at 10, 50 and 200 world units, fade shadows out at 80% of maximum shadow distance
  lightSetShadowCascade light $ CascadeParameters 10 50 200 0 0.8 1.0

  -- Create animated models
  let numModels = 100
      modelMoveSpeed = 2.0
      modelRotateSpeed = 100.0
      bounds = BoundingBox (Vector3 (-47) 0 (-47)) (Vector3 47 0 47)

  _ <- replicateM numModels $ do 

    modelNode <- nodeCreateChild scene "Jack" CM'Replicated 0
    [r1, r2] <- replicateM 2 (randomUp 90)
    nodeSetPosition modelNode $ Vector3 (r1 - 45) 0 (r2 - 45)
    r3 <- randomUp 360
    nodeSetRotation modelNode $ quaternionFromEuler 0 r3 0 

    (modelObject :: Ptr AnimatedModel) <- fromJustTrace "Jack model" <$> nodeCreateComponent modelNode Nothing Nothing
    (modelModel :: Ptr Model) <- fromJustTrace "Jack.mdl" <$> cacheGetResource cache "Models/Jack.mdl" True
    animatedModelSetModel modelObject modelModel True
    (modelMaterial :: Ptr Material) <- fromJustTrace "Jack.xml" <$> cacheGetResource cache "Materials/Jack.xml" True
    staticModelSetMaterial modelObject modelMaterial
    drawableSetCastShadows modelObject True 

    -- Create an AnimationState for a walk animation. Its time position will need to be manually updated to advance the
    -- animation, The alternative would be to use an AnimationController component which updates the animation automatically,
    -- but we need to update the model's position manually in any case
    (walkAnimation :: Ptr Animation) <- fromJustTrace "Jack_Walk.ani" <$> cacheGetResource cache "Models/Jack_Walk.ani" True 
    (state :: Ptr AnimationState) <- animatedModelAddAnimationState modelObject walkAnimation
    -- The state would fail to create (return null) if the animation was not found
    unless (isNull state) $ do 
      -- Enable full blending weight and looping
      animationStateSetWeight state 1
      animationStateSetLooped state True
      animationStateSetTime state =<< randomUp =<< animationGetLength walkAnimation

    -- Create our custom Mover component that will move & animate the model during each frame's update
    (mover :: Ptr Mover) <- fromJustTrace "Mover" <$> nodeCreateCustomComponent modelNode moverType Nothing Nothing
    setMoverParameters mover modelMoveSpeed modelRotateSpeed bounds
    return ()

  -- Create the camera. Let the starting position be at the world origin. As the fog limits maximum visible distance, we can
  -- bring the far clip plane closer for more effective culling of distant objects
  cameraNode <- nodeCreateChild scene "Camera" CM'Replicated 0
  (camera :: Ptr Camera) <- fromJustTrace "Camera component" <$> nodeCreateComponent cameraNode Nothing Nothing
  cameraSetFarClip camera 300 

  -- Set an initial position for the camera scene node above the plane
  nodeSetPosition cameraNode (Vector3 0 5 0)

  return (scene, cameraNode)

-- | Construct an instruction text to the UI.
createInstructions :: SharedApplicationPtr -> IO ()
createInstructions app = do 
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app 
  (ui :: Ptr UI) <- fromJustTrace "UI" <$> getSubsystem app
  root <- uiRoot ui 

  -- Construct new Text object, set string to display and font to use
  (instructionText :: Ptr Text) <- createChildSimple root
  textSetText instructionText "Use WASD keys and mouse/touch to move\nSpace to toggle debug geometry"
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
  (camera :: Ptr Camera) <- fromJustTrace "Camera" <$> nodeGetComponent' cameraNode False
  (viewport :: SharedViewportPtr) <- newSharedObject (cntx, pointer scene, camera)
  rendererSetViewport renderer 0 viewport

data CameraData = CameraData {
  camYaw :: Float 
, camPitch :: Float
, camDebugGeometry :: Bool
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
    let pitch = clamp (-90) 90 $ camPitch camData + mouseSensitivity * fromIntegral (mouseMove ^. y)

    -- Construct new orientation for the camera scene node from yaw and pitch. Roll is fixed to zero
    nodeSetRotation cameraNode $ quaternionFromEuler pitch yaw 0 

    -- Read WASD keys and move the camera scene node to the corresponding direction if they are pressed
    -- Use the Translate() function (default local space) to move relative to the node's orientation.
    whenM (inputGetKeyDown input KeyW) $ 
      nodeTranslate cameraNode (vec3Forward `mul` (moveSpeed * timeStep)) TS'Local
    whenM (inputGetKeyDown input KeyS) $ 
      nodeTranslate cameraNode (vec3Back `mul` (moveSpeed * timeStep)) TS'Local
    whenM (inputGetKeyDown input KeyA) $ 
      nodeTranslate cameraNode (vec3Left `mul` (moveSpeed * timeStep)) TS'Local
    whenM (inputGetKeyDown input KeyD) $ 
      nodeTranslate cameraNode (vec3Right `mul` (moveSpeed * timeStep)) TS'Local

    -- Toggle debug geometry with space
    spacePressed <- inputGetKeyPress input KeySpace

    return camData {
        camYaw = yaw 
      , camPitch = pitch
      , camDebugGeometry = (if spacePressed then not else id) $ camDebugGeometry camData
      }
  where 
    mul (Vector3 a b c) v = Vector3 (a*v) (b*v) (c*v)

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SharedApplicationPtr -> Ptr Node -> IO ()
subscribeToEvents app cameraNode = do 
  camDataRef <- newIORef $ CameraData 0 0 False
  subscribeToEvent app $ handleUpdate app cameraNode camDataRef

-- | Handle the logic update event.
handleUpdate :: SharedApplicationPtr -> Ptr Node -> IORef CameraData -> EventUpdate -> IO ()
handleUpdate app cameraNode camDataRef e = do 
  -- Take the frame time step, which is stored as a float
  let t = e ^. timeStep
  camData <- readIORef camDataRef
  -- Move the camera, scale movement with time step
  writeIORef camDataRef =<< moveCamera app cameraNode t camData

handlePostRenderUpdate :: SharedApplicationPtr -> IORef CameraData -> EventPostRenderUpdate -> IO ()
handlePostRenderUpdate app camDataRef _ = do 
  camData <- readIORef camDataRef
  (renderer :: Ptr Renderer) <- fromJustTrace "Input" <$> getSubsystem app

  -- If draw debug mode is enabled, draw viewport debug geometry, which will show eg. drawable bounding boxes and skeleton
  -- bones. Note that debug geometry has to be separately requested each frame. Disable depth test so that we can see the
  -- bones properly
  when (camDebugGeometry camData) $ rendererDrawDebugGeometry renderer False