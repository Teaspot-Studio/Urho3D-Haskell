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
  (_ :: Ptr StaticModel) <- fromJustTrace "Plane StaticModel" <$> nodeCreateComponent scene Nothing Nothing
  
  undefined

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