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
 Billboard example.
 This sample demonstrates:
     - Populating a 3D scene with billboard sets and several shadow casting spotlights
     - Parenting scene nodes to allow more intuitive creation of groups of objects
     - Examining rendering performance with a somewhat large object and light count
-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Lens hiding (Context, element)
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Proxy
import Foreign
import Graphics.Urho3D
import Sample

main :: IO ()
main = withObject () $ \cntx -> do
  newSample cntx "Billboards" joysticPatch (customStart cntx) >>= runSample

-- | Setup after engine initialization and before running the main loop.
customStart :: Ptr Context -> SampleRef -> IO ()
customStart cntx sr = do
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
  -- Save scene to prevent garbage collecting
  writeIORef sr $ sampleScene .~ scene $ s

-- | Construct the scene content.
createScene :: SharedPtr Application -> IO (SharedPtr Scene, Ptr Node)
createScene app = do
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app
  (scene :: SharedPtr Scene) <- newSharedObject =<< getContext app

  {-
    Create octree, use default volume (-1000, -1000, -1000) to (1000, 1000, 1000)
    Also create a DebugRenderer component so that we can draw debug geometry
  -}
  (_ :: Ptr Octree) <- fromJustTrace "Octree" <$> nodeCreateComponent scene Nothing Nothing
  (_ :: Ptr DebugRenderer) <- fromJustTrace "DebugRenderer" <$> nodeCreateComponent scene Nothing Nothing

  -- Create a Zone component for ambient lighting & fog control
  zoneNode <- nodeCreateChild scene "Zone" CM'Replicated 0
  (zone :: Ptr Zone) <- fromJustTrace "Zone" <$> nodeCreateComponent zoneNode Nothing Nothing
  -- Set same volume as the Octree, set a close bluish fog and some ambient light
  zoneSetBoundingBox zone $ BoundingBox (-1000) 1000
  zoneSetAmbientColor zone $ rgb 0.1 0.1 0.1
  zoneSetFogStart zone 100
  zoneSetFogEnd zone 300

  -- Create a directional light without shadows
  lightNode <- nodeCreateChild scene "DirectionalLight" CM'Replicated 0
  nodeSetDirection lightNode (Vector3 0.5 (-1.0) 0.5)
  (light :: Ptr Light) <- fromJustTrace "Light" <$> nodeCreateComponent lightNode Nothing Nothing
  lightSetLightType light LT'Directional
  lightSetColor light $ rgb 0.2 0.2 0.2
  lightSetSpecularIntensity light 1

  -- Create a "floor" consisting of several tiles
  forM [(x, y)| x <- [-5 .. 5], y <- [-5 .. 5]] $ \(x :: Int, y :: Int) -> do
    floorNode <- nodeCreateChild scene "FloorTile" CM'Replicated 0
    nodeSetPosition floorNode $ Vector3 (fromIntegral x * 20.5) (-0.5) (fromIntegral y * 20.5)
    nodeSetScale floorNode $ Vector3 20 1 20
    (floorObject :: Ptr StaticModel) <- fromJustTrace "Tile model" <$> nodeCreateComponent floorNode Nothing Nothing
    (floorModel :: Ptr Model) <- fromJustTrace "Box.mld" <$> cacheGetResource cache "Models/Box.mdl" True
    (floorMaterial :: Ptr Material) <- fromJustTrace "Stone.xml" <$> cacheGetResource cache "Materials/Stone.xml" True
    staticModelSetModel floorObject floorModel
    staticModelSetMaterial floorObject floorMaterial

  -- Create groups of mushrooms, which act as shadow casters
  let numMushroomGroups = 25
      numMushrooms = 25

  _ <- replicateM numMushroomGroups $ do
    -- First create a scene node for the group. The individual mushrooms nodes will be created as children
    (groupNode :: Ptr Node) <- nodeCreateChild scene "MushroomGroup" CM'Replicated 0
    [r1, r2] <- replicateM 2 (randomUp 190)
    nodeSetPosition groupNode $ Vector3 (r1 - 95) 0 (r2 - 95)

    replicateM numMushrooms $ do
      (mushroomNode :: Ptr Node) <- nodeCreateChild groupNode "Mushroom" CM'Replicated 0
      [r3, r4] <- replicateM 2 (randomUp 25)
      [r5, r6] <- replicateM 2 random
      nodeSetPosition mushroomNode $ Vector3 (r3 - 12.5) 0 (r4 - 12.5)
      nodeSetRotation mushroomNode $ quaternionFromEuler 0 (r5 * 360) 0
      nodeSetScale mushroomNode $ vec3 (1 + r6 * 4)

      (mushroomObject :: Ptr StaticModel) <- fromJustTrace "Mushroom model" <$> nodeCreateComponent mushroomNode Nothing Nothing
      (mushroomModel :: Ptr Model) <- fromJustTrace "Mushroom.mdl" <$> cacheGetResource cache "Models/Mushroom.mdl" True
      (mushroomMaterial :: Ptr Material) <- fromJustTrace "Mushroom.xml" <$> cacheGetResource cache "Materials/Mushroom.xml" True
      staticModelSetModel mushroomObject mushroomModel
      staticModelSetMaterial mushroomObject mushroomMaterial
      drawableSetCastShadows mushroomObject True


  -- Create billboard sets (floating smoke)
  let numBillboardNodes = 25
      numBillboards = 10

  _ <- replicateM numBillboards $ do
    (smokeNode :: Ptr Node) <- nodeCreateChild scene "Smoke" CM'Replicated 0
    [sr1, sr2] <- replicateM 2 (randomUp 200)
    sr3 <- randomUp 20
    nodeSetPosition smokeNode $ Vector3 (sr1 - 100) sr3 (sr2 - 100)

    (billboardObject :: Ptr BillboardSet) <- fromJustTrace "Smoke billboard" <$> nodeCreateComponent smokeNode Nothing Nothing
    billboardSetSetNumBillboards billboardObject numBillboards
    (smokeMaterial :: Ptr Material) <- fromJustTrace "LitSmoke.xml" <$> cacheGetResource cache "Materials/LitSmoke.xml" True
    billboardSetSetMaterial billboardObject smokeMaterial
    billboardSetSetSorted billboardObject True


    _ <- forM [0 .. numBillboards-1] $ \j -> do
      bb <- billboardSetGetBillboard billboardObject j
      [sr4, sr5] <- replicateM 2 (randomUp 12)
      sr6 <- randomUp 8
      [sr7, sr8] <- replicateM 2 (randomUp 2)
      sr9 <- randomUp 360
      billboardSetSetBillboard billboardObject j $ bb
        & position .~ Vector3 (sr4 - 6) (sr6 - 4) (sr5 - 6)
        & size .~ Vector2 (sr7 + 3.0) (sr8 + 3.0)
        & rotation .~ sr9
        & enabled .~ True

    -- After modifying the billboards, they need to be "commited" so that the BillboardSet updates its internals
    billboardSetCommit billboardObject

  -- Create shadow casting spotlights
  let numLights = 9 :: Int
  _ <- forM [0 .. numLights] $ \i -> do
    (lightNode :: Ptr Node) <- nodeCreateChild scene "SpotLight" CM'Replicated 0
    (light :: Ptr Light) <- fromJustTrace "SpotLight" <$> nodeCreateComponent lightNode Nothing Nothing

    let angle = 0 :: Float
        position = Vector3 (fromIntegral (i `mod` 3) * 60 - 60) 45 ((fromIntegral i / 3) * 60 - 60)
        color = rgb (fromIntegral ((i+1) .&. 1) * 0.5 + 0.5)
                    (fromIntegral (((i+1) `shiftR` 1) .&. 1) * 0.5 + 0.5)
                    (fromIntegral (((i+1) `shiftR` 2) .&. 1) * 0.5 + 0.5)

    nodeSetPosition lightNode position
    nodeSetDirection lightNode $ Vector3 (sin angle) (-1.5) (cos angle)

    lightSetLightType light LT'Spot
    lightSetRange light 90
    rampTexture :: Ptr Texture2D <- fromJustTrace "RampExtreme.png" <$> cacheGetResource cache "Textures/RampExtreme.png" True
    lightSetRampTexture light (castToParent rampTexture)
    lightSetFov light 45
    lightSetColor light color
    lightSetSpecularIntensity light 1
    drawableSetCastShadows light True
    lightSetShadowBias light $ BiasParameters 0.00002 0

    -- Configure shadow fading for the lights. When they are far away enough, the lights eventually become unshadowed for
    -- better GPU performance. Note that we could also set the maximum distance for each object to cast shadows
    lightSetShadowFadeDistance light 100 -- Fade start distance
    drawableSetShadowDistance light 125 -- Fade end distance, shadows are disabled
    -- Set half resolution for the shadow maps for increased performance
    lightSetShadowResolution light 0.5
    -- The spot lights will not have anything near them, so move the near plane of the shadow camera farther
    -- for better shadow depth resolution
    lightSetShadowNearFarRatio light 0.01

  -- Create the camera. Let the starting position be at the world origin. As the fog limits maximum visible distance, we can
  -- bring the far clip plane closer for more effective culling of distant objects
  cameraNode <- nodeCreateChild scene "Camera" CM'Replicated 0
  (cam :: Ptr Camera) <- fromJustTrace "Camera component" <$> nodeCreateComponent cameraNode Nothing Nothing
  cameraSetFarClip cam 300

  -- Set an initial position for the camera scene node above the plane
  nodeSetPosition cameraNode (Vector3 0 5 0)

  return (scene, cameraNode)

-- | Construct an instruction text to the UI.
createInstructions :: SharedPtr Application -> IO ()
createInstructions app = do
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app
  (ui :: Ptr UI) <- fromJustTrace "UI" <$> getSubsystem app
  roote <- uiRoot ui

  -- Construct new Text object, set string to display and font to use
  (instructionText :: Ptr Text) <- createChildSimple roote
  textSetText instructionText "Use WASD keys and mouse/touch to move\nSpace to toggle debug geometry"
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
, camDebugGeometry :: Bool
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
    let pitch = clamp (-90) 90 $ camPitch camData + mouseSensitivity * fromIntegral (mouseMove ^. y)

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

    -- Toggle debug geometry with space
    spacePressed <- inputGetKeyPress input KeySpace

    return camData {
        camYaw = yaw
      , camPitch = pitch
      , camDebugGeometry = (if spacePressed then not else id) $ camDebugGeometry camData
      }
  where
    mul (Vector3 a b c) v = Vector3 (a*v) (b*v) (c*v)

-- | Rotate lights and billboards
animateScene :: SharedPtr Application -> Float -> IO ()
animateScene app timeStep = do
  (scene :: SharedPtr Scene) <- newSharedObject =<< getContext app
  lightNodes :: [Ptr Node] <- nodeGetChildrenWithComponent scene (Proxy @Light) False
  billboardNodes :: [Ptr Node] <- nodeGetChildrenWithComponent scene (Proxy @BillboardSet) False
  print $ length lightNodes

  let lightRotationSpeed = 20
      billboardRotationSpeed = 50

  -- Rotate the lights around the world Y-axis
  forM_ lightNodes $ \node ->
    nodeRotate node (quaternionFromEuler 0 (lightRotationSpeed * timeStep) 0) TS'World

  -- Rotate the individual billboards within the billboard sets, then recommit to make the changes visible
  forM_ billboardNodes $ \node -> do
    mc <- nodeGetComponent node False
    whenJust mc $ \(bset :: Ptr BillboardSet) -> do
      n <- billboardSetGetNumBillboards bset
      forM_ [0 .. n-1] $ \i -> do
        bb <- billboardSetGetBillboard bset i
        billboardSetSetBillboard bset i $ bb
          & rotation +~ billboardRotationSpeed * timeStep
      billboardSetCommit bset

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SharedPtr Application -> Ptr Node -> IO ()
subscribeToEvents app cameraNode = do
  camDataRef <- newIORef $ CameraData 0 0 False
  subscribeToEvent app $ handleUpdate app cameraNode camDataRef
  subscribeToEvent app $ handlePostRenderUpdate app camDataRef

-- | Handle the logic update event.
handleUpdate :: SharedPtr Application -> Ptr Node -> IORef CameraData -> EventUpdate -> IO ()
handleUpdate app cameraNode camDataRef e = do
  -- Take the frame time step, which is stored as a float
  let t = e ^. timeStep
  camData <- readIORef camDataRef
  -- Move the camera and animate the scene, scale movement with time step
  writeIORef camDataRef =<< moveCamera app cameraNode t camData
  animateScene app t

handlePostRenderUpdate :: SharedPtr Application -> IORef CameraData -> EventPostRenderUpdate -> IO ()
handlePostRenderUpdate app camDataRef _ = do
  camData <- readIORef camDataRef
  (renderer :: Ptr Renderer) <- fromJustTrace "Input" <$> getSubsystem app

  -- If draw debug mode is enabled, draw viewport debug geometry, which will show eg. drawable bounding boxes and skeleton
  -- bones. Note that debug geometry has to be separately requested each frame. This time use depth test, as otherwise the result becomes
  -- hard to interpret due to large object count
  when (camDebugGeometry camData) $
    rendererDrawDebugGeometry renderer True
