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
  createScene app 
  -- Create the UI content 
  createInstructions app 
  -- Setup the viewport for displaying the scene
  setupViewport app 
  -- Hook up to the frame update events 
  subscribeToEvents app 

-- | Construct the scene content.
createScene :: SharedApplicationPtr -> IO SharedScenePtr
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
  (planeObject :: Ptr StaticModel) <- fromJustTrace "Plane StaticModel" <$> nodeCreateComponent scene Nothing Nothing
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

    (mushroomObject :: Ptr StaticModel) <- fromJustTrace "Mushroom StaticModel" <$> nodeCreateComponent scene Nothing Nothing
    (mushroomModel :: Ptr Model) <- fromJustTrace "Mushroom.mdl" <$> cacheGetResource cache "Models/Mushroom.mdl" True
    staticModelSetModel mushroomObject mushroomModel
    (mushroomMaterial :: Ptr Material) <- fromJustTrace "Mushroom.xml" <$> cacheGetResource cache "Materials/Mushroom.xml" True
    staticModelSetMaterial mushroomObject mushroomMaterial

  {-
    Create a scene node for the camera, which we will move around
    The camera will use default settings (1000 far clip distance, 45 degrees FOV, set aspect ratio automatically)
  -}
  cameraNode <- nodeCreateChild scene "Camera" CM'Replicated 0
  (_ :: Ptr Camera) <- fromJustTrace "Camera component" <$> nodeCreateComponent scene Nothing Nothing

  -- Set an initial position for the camera scene node above the plane
  nodeSetPosition cameraNode $ Vector3 0 5.0 0

  return scene
  
-- | Construct an instruction text to the UI.
createInstructions :: SharedApplicationPtr -> IO ()
createInstructions app = undefined

-- | Set up a viewport for displaying the scene.
setupViewport :: SharedApplicationPtr -> IO ()
setupViewport app = undefined

-- | Read input and moves the camera.
moveCamera :: Float -> IO ()
moveCamera timeStep = undefined

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SharedApplicationPtr -> IO ()
subscribeToEvents app = undefined

-- | Handle the logic update event.
handleUpdate :: EventUpdate -> IO ()
handleUpdate e = undefined