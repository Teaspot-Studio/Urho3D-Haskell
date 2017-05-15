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
-- Dynamic geometry example.
-- This sample demonstrates:
--     - Cloning a Model resource
--     - Modifying the vertex buffer data of the cloned models at runtime to efficiently animate them
--     - Creating a Model resource and its buffer data from scratch

{-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Lens hiding (Context, element)
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Maybe
import Data.Proxy
import Data.Store.Core
import Foreign
import Graphics.Urho3D
import Sample

import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

main :: IO ()
main = withObject () $ \cntx -> do
  newSample cntx "Billboards" joysticPatch (customStart cntx) >>= runSample

-- | Setup after engine initialization and before running the main loop.
customStart :: Ptr Context -> SampleRef -> IO ()
customStart cntx sr = do
  s <- readIORef sr
  let app = s ^. sampleApplication

  -- Create the scene content
  (scene, cameraNode, adata) <- createScene app
  -- Create the UI content
  createInstructions app
  -- Setup the viewport for displaying the scene
  setupViewport app scene cameraNode
  -- Hook up to the frame update events
  subscribeToEvents sr cameraNode adata
  -- Save scene to prevent garbage collecting
  writeIORef sr $ sampleScene .~ scene $ s
  -- Set the mouse mode to use in the sample
  initMouseMode sr MM'Relative

-- | Data that is needed for animation
data AnimateData = AnimateData {
  originalVertices :: V.Vector Vector3
, vertexDuplicates :: V.Vector Int
, animatingBuffers :: V.Vector (SharedPtr VertexBuffer)
}

-- | Construct the scene content.
createScene :: SharedPtr Application -> IO (SharedPtr Scene, Ptr Node, AnimateData)
createScene app = do
  cache :: Ptr ResourceCache <- fromJustTrace "ResourceCache" <$> getSubsystem app
  scene :: SharedPtr Scene <- newSharedObject =<< getContext app

  {-
    Create octree, use default volume (-1000, -1000, -1000) to (1000, 1000, 1000)
    Also create a DebugRenderer component so that we can draw debug geometry
  -}
  _ :: Ptr Octree <- fromJustTrace "Octree" <$> nodeCreateComponent scene Nothing Nothing

  -- Create a Zone component for ambient lighting & fog control
  zoneNode <- nodeCreateChild scene "Zone" CM'Replicated 0
  zone :: Ptr Zone <- fromJustTrace "Zone" <$> nodeCreateComponent zoneNode Nothing Nothing
  -- Set same volume as the Octree, set a close bluish fog and some ambient light
  zoneSetBoundingBox zone $ BoundingBox (-1000) 1000
  zoneSetFogColor zone $ rgb 0.2 0.2 0.2
  zoneSetFogStart zone 200
  zoneSetFogEnd zone 300

  -- Create a directional light
  lightNode <- nodeCreateChild scene "DirectionalLight" CM'Replicated 0
  nodeSetDirection lightNode (Vector3 (-0.6) (-1.0) (-0.8)) -- The direction vector does not need to be normalized
  light :: Ptr Light <- fromJustTrace "Light" <$> nodeCreateComponent lightNode Nothing Nothing
  lightSetLightType light LT'Directional
  lightSetColor light $ rgb 0.4 1.0 0.4
  lightSetSpecularIntensity light 1.5

  --  Get the original model and its unmodified vertices, which are used as source data for the animation
  originalModel :: Ptr Model <- fromJustTrace "Box.mld" <$> cacheGetResource cache "Models/Box.mdl" True
  -- Get the vertex buffer from the first geometry's first LOD level
  geometry <- fromJustTrace "Geometry" <$> modelGetGeometry originalModel 0 0
  buffer <- fromJustTrace "Vertex buffer" <$> geometryGetVertexBuffer geometry 0
  numVertices <- vertexBufferGetVertexCount buffer
  mVertexData <- fmap castPtr <$> vertexBufferLock buffer 0 numVertices False
  -- Copy the original vertex positions
  originalVertices :: V.Vector Vector3 <- case mVertexData of
    Nothing -> pure mempty
    Just vertexData -> do
      vertexSize <- fromIntegral <$> vertexBufferGetVertexSize buffer
      originalVertices <- V.generateM (fromIntegral numVertices) $ \i -> peek $ plusPtr vertexData (i * vertexSize)
      vertexBufferUnlock buffer
      pure originalVertices

  -- Detect duplicate vertices to allow seamless animation
  let vertexDuplicates = V.generate (V.length originalVertices) $ \i ->
        let v1 = originalVertices V.! i
            checkDuplicate (j, v2) Nothing = if v1 == v2 then Just j else Nothing
            checkDuplicate _ acc = acc
        in fromMaybe i . V.foldr' checkDuplicate Nothing . V.indexed . V.take i $ originalVertices

  -- Create StaticModels in the scene. Clone the model for each so that we can modify the vertex data individually
  animatingBuffers <- V.generateM 9 $ \i -> do
    let x = (i `mod` 3) - 1
        y = (i `div` 3) - 1
    node <- nodeCreateChild scene "Object" CM'Replicated 0
    nodeSetPosition node $ Vector3 (fromIntegral x * 2) 0 (fromIntegral y * 2)
    object :: Ptr StaticModel <- fromJustTrace "Object model" <$> nodeCreateComponent node Nothing Nothing
    cloneModel <- modelClone originalModel ""
    staticModelSetModel object cloneModel
    -- Store the cloned vertex buffer that we will modify when animating
    g <- fromJustTrace "Object model geometry" <$> modelGetGeometry cloneModel 0 0
    vb <- fromJustTrace "Object geometry vertexbuffer" <$> geometryGetVertexBuffer g 0
    pure (makePointer vb :: SharedPtr VertexBuffer)

  -- Finally create one model (pyramid shape) and a StaticModel to display it from scratch
  -- Note: there are duplicated vertices to enable face normals. We will calculate normals programmatically
  createCustomModel scene =<< getContext app

  -- Create the camera. Let the starting position be at the world origin. As the fog limits maximum visible distance, we can
  -- bring the far clip plane closer for more effective culling of distant objects
  cameraNode <- nodeCreateChild scene "Camera" CM'Replicated 0
  nodeSetPosition cameraNode (Vector3 0 2 (-20))
  cam :: Ptr Camera <- fromJustTrace "Camera component" <$> nodeCreateComponent cameraNode Nothing Nothing
  cameraSetFarClip cam 300

  return (scene, cameraNode, AnimateData{..})

-- | Encode buffer and use it in context
withBuffer :: Poke () -> Int -> (Ptr () -> IO a)-> IO a
withBuffer p n io = BS.unsafeUseAsCString (unsafeEncodeWith p n) $ io . castPtr

-- | Finally create one model (pyramid shape) and a StaticModel to display it from scratch
-- Note: there are duplicated vertices to enable face normals. We will calculate normals programmatically
createCustomModel :: SharedPtr Scene -> Ptr Context -> IO ()
createCustomModel scene context  = do
  let numVertices = 18
      vertexData = [
          Vector3   0.0    0.5    0.0
        , Vector3   0.5  (-0.5)   0.5
        , Vector3   0.5  (-0.5) (-0.5)

        , Vector3   0.0    0.5    0.0
        , Vector3 (-0.5) (-0.5)   0.5
        , Vector3   0.5  (-0.5)   0.5

        , Vector3   0.0    0.5    0.0
        , Vector3 (-0.5) (-0.5) (-0.5)
        , Vector3 (-0.5) (-0.5)   0.5

        , Vector3   0.0    0.5    0.0
        , Vector3   0.5  (-0.5) (-0.5)
        , Vector3 (-0.5) (-0.5) (-0.5)

        , Vector3   0.5  (-0.5) (-0.5)
        , Vector3   0.5  (-0.5)   0.5
        , Vector3 (-0.5) (-0.5)   0.5

        , Vector3   0.5  (-0.5) (-0.5)
        , Vector3 (-0.5) (-0.5)   0.5
        , Vector3 (-0.5) (-0.5) (-0.5)
        ]
      indexData = [
          0, 1, 2,
          3, 4, 5,
          6, 7, 8,
          9, 10, 11,
          12, 13, 14,
          15, 16, 17 :: Word16
        ]
      -- Calculate face normals now
      normalData = V.generate (V.length vertexData `div` 3) $ \i -> let
        v1 = vertexData V.! i
        v2 = vertexData V.! i+1
        v3 = vertexData V.! i+2
        edge1 = v1 - v2
        edge2 = v1 - v3
        in vec3Normalize $ edge1 `vec3Cross` edge2

  fromScratchModel :: SharedPtr Model <- newSharedObject $ pointer context
  vb :: SharedPtr VertexBuffer <- newSharedObject $ pointer context
  ib :: SharedPtr IndexBuffer <- newSharedObject $ pointer context
  geom :: SharedPtr Geometry  <- newSharedObject $ pointer context

  -- Shadowed buffer needed for raycasts to work, and so that data can be automatically restored on device loss
  vertexBufferSetShadowed vb True
  -- We could use the "legacy" element bitmask to define elements for more compact code, but let's demonstrate
  -- defining the vertex elements explicitly to allow any element types and order
  let elements = [ vertexElement Type'Vector3 SEM'Position
                 , vertexElement Type'Vector3 SEM'Normal ]
  vertexBufferSetSize vb numVertices elements False
  let mkBuffer = V.mapM_ storeRow $ V.indexed vertexData
      storeRow (i, v) = do
        pokeStorable v
        pokeStorable $ normalData V.! (i `div` 3)
  _ <- withBuffer mkBuffer (fromIntegral numVertices * 2 * sizeOf (undefined :: Vector3)) $ vertexBufferSetData vb

  indexBufferSetShadowed ib True
  indexBufferSetSize ib numVertices False False
  let mkIndexBuffer = V.mapM_ pokeStorable indexData
  withBuffer mkIndexBuffer (fromIntegral numVertices * sizeOf (undefined :: Word16)) $ indexBufferSetData ib

  geometrySetVertexBuffer geom 0 (pointer vb)
  geometrySetIndexBuffer geom (pointer ib)
  geometrySetDrawRange geom TriangleList 0 numVertices True

  _ <- modelSetNumGeometries fromScratchModel 1
  _ <- modelSetGeometry fromScratchModel 0 0 (pointer geom)
  modelSetBoundingBox fromScratchModel $ BoundingBox (Vector3 (-0.5) (-0.5) (-0.5)) (Vector3 0.5 0.5 0.5)

  -- Though not necessary to render, the vertex & index buffers must be listed in the model so that it can be saved properly
  let vertexBuffers = [vb]
      indexBuffers = [ib]
  -- Morph ranges could also be not defined. Here we simply define a zero range (no morphing) for the vertex buffer
      morphRangeStarts = [0]
      morphRangeCounts = [0]
  modelSetVertexBuffers fromScratchModel vertexBuffers morphRangeStarts morphRangeCounts
  modelSetIndexBuffers fromScratchModel indexBuffers

  node <- nodeCreateChild scene "FromScratchObject" CM'Replicated 0
  nodeSetPosition node $ Vector3 0 3 0
  object :: Ptr StaticModel <- fromJustTrace "FromScratchObject model" <$> nodeCreateComponent node Nothing Nothing
  staticModelSetModel object fromScratchModel

-- | Construct an instruction text to the UI.
createInstructions :: SharedPtr Application -> IO ()
createInstructions app = do
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app
  (ui :: Ptr UI) <- fromJustTrace "UI" <$> getSubsystem app
  roote <- uiRoot ui

  -- Construct new Text object, set string to display and font to use
  (instructionText :: Ptr Text) <- createChildSimple roote
  textSetText instructionText "Use WASD keys and mouse/touch to move\nSpace to toggle animation"
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
animateScene :: SampleRef -> IORef Float -> Float -> AnimateData -> IO ()
animateScene sr timeRef timeStep AnimateData{..} = do
  sr <- readIORef sr
  let scene = sr ^. sampleScene
  modifyIORef' timeRef (+ (timeStep * 100))
  time <- readIORef timeRef

  -- Repeat for each of the cloned vertex buffers
  forM_ (V.indexed animatingBuffers) $ \(i, buffer) -> do
    let startPhase = time + fromIntegral i  * 30
    -- Lock the vertex buffer for update and rewrite positions with sine wave modulated ones
    -- Cannot use discard lock as there is other data (normals, UVs) that we are not overwriting
    cnt <- vertexBufferGetVertexCount buffer
    mVertexData <- fmap castPtr <$> vertexBufferLock buffer 0 cnt False
    whenJust mVertexData $ \vertexData -> do
      vertexSize <- vertexBufferGetVertexSize buffer
      numVertices <- vertexBufferGetVertexCount buffer
      forM_ (V.indexed originalVertices) $ \(j, src) -> do
        -- If there are duplicate vertices, animate them in phase of the original
        let phase = startPhase + fromIntegral (vertexDuplicates V.! j) * 10
            dest = Vector3 (src ^. x * (1.0 + 0.1 * sin phase))
                          (src ^. y * (1.0 + 0.1 * sin (phase + 60)))
                          (src ^. z * (1.0 + 0.1 * sin (phase + 120)))
            ptr = vertexData `plusPtr` (j * fromIntegral vertexSize)
        poke ptr dest
      vertexBufferUnlock buffer

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SampleRef -> Ptr Node -> AnimateData -> IO ()
subscribeToEvents sr cameraNode adata = do
  s <- readIORef sr
  let app = s ^. sampleApplication
  camDataRef <- newIORef $ CameraData 0 0 False
  timeRef <- newIORef 0
  subscribeToEvent app $ handleUpdate sr cameraNode camDataRef timeRef adata
  subscribeToEvent app $ handlePostRenderUpdate app camDataRef

-- | Handle the logic update event.
handleUpdate :: SampleRef -> Ptr Node -> IORef CameraData -> IORef Float -> AnimateData -> EventUpdate -> IO ()
handleUpdate sr cameraNode camDataRef timeRef adata e = do
  s <- readIORef sr
  let app = s ^. sampleApplication
  -- Take the frame time step, which is stored as a float
  let t = e ^. timeStep
  camData <- readIORef camDataRef
  -- Move the camera and animate the scene, scale movement with time step
  writeIORef camDataRef =<< moveCamera app cameraNode t camData
  animateScene sr timeRef t adata

handlePostRenderUpdate :: SharedPtr Application -> IORef CameraData -> EventPostRenderUpdate -> IO ()
handlePostRenderUpdate app camDataRef _ = do
  camData <- readIORef camDataRef
  (renderer :: Ptr Renderer) <- fromJustTrace "Input" <$> getSubsystem app

  -- If draw debug mode is enabled, draw viewport debug geometry, which will show eg. drawable bounding boxes and skeleton
  -- bones. Note that debug geometry has to be separately requested each frame. This time use depth test, as otherwise the result becomes
  -- hard to interpret due to large object count
  when (camDebugGeometry camData) $
    rendererDrawDebugGeometry renderer True
