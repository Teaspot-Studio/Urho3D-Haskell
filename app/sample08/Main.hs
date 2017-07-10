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
 Decals example.
 This sample demonstrates:
     - Performing a raycast to the octree and adding a decal to the hit location
     - Defining a Cursor UI element which stays inside the window and can be shown/hidden
     - Marking suitable (large) objects as occluders for occlusion culling
     - Displaying renderer debug geometry to see the effect of occlusion
-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Lens hiding (Context, element)
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Maybe
import Data.Proxy
import Foreign hiding (void)
import Graphics.Urho3D
import Sample

main :: IO ()
main = withObject () $ \cntx ->
  newSample cntx "Billboards" joysticPatch (customStart cntx) >>= runSample

-- | Setup after engine initialization and before running the main loop.
customStart :: Ptr Context -> SampleRef -> IO ()
customStart cntx sr = do
  s <- readIORef sr
  let app = s ^. sampleApplication

  -- Create the scene content
  (scene, cameraNode) <- createScene app
  -- Create the UI content
  createUI app
  -- Setup the viewport for displaying the scene
  setupViewport app scene cameraNode
  -- Hook up to the frame update events
  subscribeToEvents sr cameraNode
  -- Save scene to prevent garbage collecting
  writeIORef sr $ sampleScene .~ scene $ s

  -- Set the mouse mode to use in the sample
  initMouseMode sr MM'Relative

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

  -- Create scene node & StaticModel component for showing a static plane
  planeNode <- nodeCreateChild scene "Plane" CM'Local 0
  nodeSetScale planeNode $ Vector3 100 1 100
  planeObject :: Ptr StaticModel <- fromJustTrace "Plane" <$> nodeCreateComponent planeNode Nothing Nothing
  planeModel :: Ptr Model <- fromJustTrace "Plane.mdl" <$> cacheGetResource cache "Models/Plane.mdl" True
  planeMaterial :: Ptr Material <- fromJustTrace "StoneTiled.xml" <$> cacheGetResource cache "Materials/StoneTiled.xml" True
  staticModelSetModel planeObject planeModel
  staticModelSetMaterial planeObject planeMaterial

  -- Create a Zone component for ambient lighting & fog control
  zoneNode <- nodeCreateChild scene "Zone" CM'Local 0
  (zone :: Ptr Zone) <- fromJustTrace "Zone" <$> nodeCreateComponent zoneNode Nothing Nothing
  -- Set same volume as the Octree, set a close bluish fog and some ambient light
  zoneSetBoundingBox zone $ BoundingBox (-1000) 1000
  zoneSetAmbientColor zone $ rgb 0.15 0.15 0.15
  zoneSetFogColor zone $ rgb 0.5 0.5 0.7
  zoneSetFogStart zone 100
  zoneSetFogEnd zone 300

  -- Create a directional light without shadows
  lightNode <- nodeCreateChild scene "DirectionalLight" CM'Replicated 0
  nodeSetDirection lightNode (Vector3 0.6 (-1.0) 0.8)
  (light :: Ptr Light) <- fromJustTrace "Light" <$> nodeCreateComponent lightNode Nothing Nothing
  lightSetLightType light LT'Directional
  drawableSetCastShadows light True
  lightSetShadowBias light $ BiasParameters 0.00025 0.5
  -- Set cascade splits at 10, 50 and 200 world units, fade shadows out at 80% of maximum shadow distance
  lightSetShadowCascade light $ CascadeParameters 10 50 200 1000 0 0.8

  -- Create some mushrooms
  let numMushrooms = 240

  replicateM_ numMushrooms $ do
    mushroomNode :: Ptr Node <- nodeCreateChild scene "Mushroom" CM'Local 0
    r1 <- randomUp 90
    r2 <- randomUp 90
    nodeSetPosition mushroomNode $ Vector3 (r1 - 45) 0 (r2 - 45)
    r3 <- randomUp 360
    nodeSetRotation mushroomNode $ quaternionFromEuler 0 r3 0

    mushroomObject :: Ptr StaticModel <- fromJustTrace "Mushroom model" <$> nodeCreateComponent mushroomNode Nothing Nothing
    mushroomModel :: Ptr Model <- fromJustTrace "Mushroom.mdl" <$> cacheGetResource cache "Models/Mushroom.mdl" True
    mushroomMaterial :: Ptr Material <- fromJustTrace "Mushroom.xml" <$> cacheGetResource cache "Materials/Mushroom.xml" True
    staticModelSetModel mushroomObject mushroomModel
    staticModelSetMaterial mushroomObject mushroomMaterial
    drawableSetCastShadows mushroomObject True

  -- Create randomly sized boxes. If boxes are big enough, make them occluders. Occluders will be software rasterized before
  -- rendering to a low-resolution depth-only buffer to test the objects in the view frustum for visibility
  let numBoxes = 20
  replicateM_ numBoxes $ do
    boxNode <- nodeCreateChild scene "Box" CM'Local 0
    r1 <- randomUp 10
    let size = 1 + r1
    r2 <- randomUp 80
    r3 <- randomUp 80
    nodeSetPosition boxNode $ Vector3 (r2 - 40) (size * 0.5) (r3 - 40)
    nodeSetScale boxNode $ Vector3 size size size
    boxObject :: Ptr StaticModel <- fromJustTrace "Box model" <$> nodeCreateComponent boxNode Nothing Nothing
    boxModel :: Ptr Model <- fromJustTrace "Box.mdl" <$> cacheGetResource cache "Models/Box.mdl" True
    boxMaterial :: Ptr Material <- fromJustTrace "Stone.xml" <$> cacheGetResource cache "Materials/Stone.xml" True
    staticModelSetModel boxObject boxModel
    staticModelSetMaterial boxObject boxMaterial
    drawableSetCastShadows boxObject True
    when (size > 3) $ drawableSetOccluder boxObject True

  -- Create the camera. Limit far clip distance to match the fog
  cameraNode <- nodeCreateChild scene "Camera" CM'Local 0
  (cam :: Ptr Camera) <- fromJustTrace "Camera component" <$> nodeCreateComponent cameraNode Nothing Nothing
  cameraSetFarClip cam 300

  -- Set an initial position for the camera scene node above the plane
  nodeSetPosition cameraNode (Vector3 0 5 0)

  return (scene, cameraNode)

-- | Construct an instruction text to the UI.
createUI :: SharedPtr Application -> IO ()
createUI app = do
  cache :: Ptr ResourceCache <- fromJustTrace "ResourceCache" <$> getSubsystem app
  ui :: Ptr UI <- fromJustTrace "UI" <$> getSubsystem app
  roote <- uiRoot ui
  context <- getContext app

  -- Create a Cursor UI element because we want to be able to hide and show it at will. When hidden, the mouse cursor will
  -- control the camera, and when visible, it will point the raycast target
  style :: Ptr XMLFile <- fromJustTrace "DefaultStyle.xml" <$> cacheGetResource cache "UI/DefaultStyle.xml" True
  cursor :: SharedPtr Cursor <- newSharedObject context
  uiElementSetStyleAuto cursor style
  uiSetCursor ui $ pointer cursor

  -- Set starting position of the cursor at the rendering window center
  graphics :: Ptr Graphics <- fromJustTrace "Graphics" <$> getSubsystem app
  w <- graphicsGetWidth graphics
  h <- graphicsGetHeight graphics
  uiElementSetPosition cursor $ IntVector2 (w `div` 2) (h `div` 2)

  -- Construct new Text object, set string to display and font to use
  instructionText :: Ptr Text <- createChildSimple roote
  textSetText instructionText "Use WASD keys to move\nLMB to paint decals, RMB to rotate view\nSpace to toggle debug geometry\n7 to toggle occlusion culling"
  font :: Ptr Font <- fromJustTrace "Anonymous Pro.ttf" <$> cacheGetResource cache "Fonts/Anonymous Pro.ttf" True
  textSetFont instructionText font 15
  -- The text has multiple rows. Center them in relation to each other
  textSetTextAlignment instructionText AlignmentHorizontalCenter

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
    cursor <- uiCursor ui
    isVisible <- uiElementIsVisible cursor
    (yaw, pitch) <- if isVisible then do
        mouseMove <- inputGetMouseMove input
        let yaw = camYaw camData + mouseSensitivity * fromIntegral (mouseMove ^. x)
        let pitch = clamp (-90) 90 $ camPitch camData + mouseSensitivity * fromIntegral (mouseMove ^. y)

        -- Construct new orientation for the camera scene node from yaw and pitch. Roll is fixed to zero
        nodeSetRotation cameraNode $ quaternionFromEuler pitch yaw 0
        pure (yaw, pitch)
      else pure (camYaw camData, camPitch camData)

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

    -- Paint decal with the left mousebutton; cursor must be visible
    isMouseBtn <- inputGetMouseButtonPress input mouseButtonLeft
    when (isVisible && isMouseBtn) $ paintDecal app cameraNode

    return camData {
        camYaw = yaw
      , camPitch = pitch
      , camDebugGeometry = (if spacePressed then not else id) $ camDebugGeometry camData
      }
  where
    mul (Vector3 a b c) v = Vector3 (a*v) (b*v) (c*v)

paintDecal :: SharedPtr Application -> Ptr Node -> IO ()
paintDecal app cameraNode = do
  mres <- raycast app cameraNode 250
  void $ whenJust mres $ \(hitPos, hitDrawable) -> do
    -- Check if target scene node already has a DecalSet component. If not, create now
    targetNode <- componentGetNode hitDrawable
    mdecal :: Maybe (Ptr DecalSet) <- nodeGetComponent targetNode True
    decal <- case mdecal of
      Nothing -> do
        cache :: Ptr ResourceCache <- fromJustTrace "ResourceCache" <$> getSubsystem app
        decal :: Ptr DecalSet <- fromJustTrace "DecalSet" <$> nodeCreateComponent targetNode Nothing Nothing
        decalMat :: Ptr Material <- fromJustTrace "UrhoDecal.xml" <$> cacheGetResource cache "Materials/UrhoDecal.xml" True
        decalSetSetMaterial decal decalMat
        pure decal
      Just decal -> pure decal

    -- Add a square decal to the decal set using the geometry of the drawable that was hit, orient it to face the camera,
    -- use full texture UV's (0,0) to (1,1). Note that if we create several decals to a large object (such as the ground
    -- plane) over a large area using just one DecalSet component, the decals will all be culled as one unit. If that is
    -- undesirable, it may be necessary to create more than one DecalSet based on the distance
    camRot <- nodeGetRotation cameraNode
    decalSetAddDecal decal hitDrawable hitPos camRot 0.5 1.0 1.0 0 1

raycast :: SharedPtr Application -> Ptr Node -> Float -> IO (Maybe (Vector3, Ptr Drawable))
raycast app cameraNode maxDistance = do
  ui :: Ptr UI <- fromJustTrace "UI" <$> getSubsystem app
  pos <- uiGetCursorPosition ui
  -- Check the cursor is visible and there is no UI element in front of the cursor
  cursor <- uiCursor ui
  isVisible <- uiElementIsVisible cursor
  mres <- uiGetElementAt ui pos True
  if not isVisible || isJust mres then pure Nothing
    else do
      graphics :: Ptr Graphics <- fromJustTrace "Graphics" <$> getSubsystem app
      camera :: Ptr Camera <- fromJustTrace "Camera" <$> nodeGetComponent cameraNode True
      width <- graphicsGetWidth graphics
      height <- graphicsGetHeight graphics
      cameraRay <- cameraGetScreenRay camera (fromIntegral (pos ^. x) / fromIntegral width) (fromIntegral (pos ^. y) / fromIntegral height)
      -- Pick only geometry objects, not eg. zones or lights, only get the first (closest) hit
      -- withObject ()
      -- query <- newObject
      undefined

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SampleRef -> Ptr Node -> IO ()
subscribeToEvents sr cameraNode = do
  s <- readIORef sr
  let app = s ^. sampleApplication
  camDataRef <- newIORef $ CameraData 0 0 False
  subscribeToEvent app $ handleUpdate sr cameraNode camDataRef
  subscribeToEvent app $ handlePostRenderUpdate app camDataRef

-- | Handle the logic update event.
handleUpdate :: SampleRef -> Ptr Node -> IORef CameraData -> EventUpdate -> IO ()
handleUpdate sr cameraNode camDataRef e = do
  s <- readIORef sr
  let app = s ^. sampleApplication
  -- Take the frame time step, which is stored as a float
  let t = e ^. timeStep
  camData <- readIORef camDataRef
  -- Move the camera and animate the scene, scale movement with time step
  writeIORef camDataRef =<< moveCamera app cameraNode t camData

handlePostRenderUpdate :: SharedPtr Application -> IORef CameraData -> EventPostRenderUpdate -> IO ()
handlePostRenderUpdate app camDataRef _ = do
  camData <- readIORef camDataRef
  (renderer :: Ptr Renderer) <- fromJustTrace "Input" <$> getSubsystem app

  -- If draw debug mode is enabled, draw viewport debug geometry, which will show eg. drawable bounding boxes and skeleton
  -- bones. Note that debug geometry has to be separately requested each frame. This time use depth test, as otherwise the result becomes
  -- hard to interpret due to large object count
  when (camDebugGeometry camData) $
    rendererDrawDebugGeometry renderer True
