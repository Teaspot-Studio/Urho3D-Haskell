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
 Moving sprites example.
 This sample demonstrates:
     - Adding Sprite elements to the UI
     - Storing custom data (sprite velocity) inside UI elements
     - Handling frame update events in which the sprites are moved
-}
module Main where

import Control.Lens hiding (Context, element)
import Control.Monad 
import Data.IORef
import Foreign
import Graphics.Urho3D
import Sample

main :: IO ()
main = withObject () $ \cntx -> do 
  newSample cntx "Sprites" joysticPatch customStart >>= runSample

-- Number of sprites to draw
numSprites :: Int 
numSprites = 100 

-- Custom vairable identifier for storing velocity within the UI element
varVelocity :: String 
varVelocity = "Velocity"

-- | Setup after engine initialization and before running the main loop.
customStart :: SampleRef -> IO ()
customStart sr = do 
  s <- readIORef sr 
  let app = s^.sampleApplication

  -- Create the sprites to the user interface
  srpites <- createSprites app
  -- Hook up to the frame update events
  subscribeToEvents app srpites

-- | Construct the sprites.
createSprites :: SharedPtr Application -> IO [SharedPtr Sprite]
createSprites app = do 
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app 
  (graphics :: Ptr Graphics) <- fromJustTrace "Graphics" <$> getSubsystem app 
  (ui :: Ptr UI) <- fromJustTrace "UI" <$> getSubsystem app 

  -- Get rendering window size as floats
  gw <- fromIntegral <$> graphicsGetWidth graphics
  gh <- fromIntegral <$> graphicsGetHeight graphics

  -- Get the Urho3D fish texture
  (decalTex :: Ptr Texture2D) <- fromJustTrace "UrhoDecal.dds" <$> cacheGetResource cache "Textures/UrhoDecal.dds" True 

  roote <- uiRoot ui 
  forM [1 .. numSprites] $ const $ do 
    -- Create a new sprite, set it to use the texture
    (sprite :: SharedPtr Sprite) <- newSharedObject =<< getContext app
    spriteSetTexture sprite decalTex

    -- The UI root element is as big as the rendering window, set random position within it 
    [r1, r2] <- replicateM 2 random
    spriteSetPosition sprite $ Vector2 (r1 * gw) (r2 * gh) 

    -- Set sprite size & hotspot in its center
    uiElementSetSize sprite 128
    spriteSetHotSpot sprite 64

    -- Set random rotation in degrees and random scale 
    rot <- random 
    spriteSetRotation sprite $ 360 * rot
    s <- randomUp 1.0
    spriteSetScale' sprite (s + 0.5) (s + 0.5)

    -- Set random color and additive blending mode
    [cr1, cr2, cr3] <- replicateM 3 $ randomUp 0.5
    uiElementSetColor sprite $ rgb (cr1 + 0.5) (cr2 + 0.5) (cr3 + 0.5)
    spriteSetBlendMode sprite BlendAdd 

    -- Add as a child of the root UI element
    uiElementAddChild roote sprite 

    -- Store sprite's velocity as a custom variable 
    [vr1, vr2] <- replicateM 2 $ randomUp 200
    uiElementSetVar sprite varVelocity $ Vector2 (vr1 - 100) (vr2 - 100)

    -- Store sprites to our own container for easy movement update iteration
    return sprite 

-- | Move the sprites using the delta time step given.
moveSprites :: SharedPtr Application -> [SharedPtr Sprite] -> Float -> IO ()
moveSprites app sprites t = do 
  (graphics :: Ptr Graphics) <- fromJustTrace "Graphics" <$> getSubsystem app 
  gw <- fromIntegral <$> graphicsGetWidth graphics
  gh <- fromIntegral <$> graphicsGetHeight graphics 

  -- Go through all sprites
  forM_ sprites $ \sprite -> do 
    -- Rotate 
    rot <- spriteGetRotation sprite
    spriteSetRotation sprite $ t * 30 + rot

    -- Move, wrap around rendering window edges 
    p <- spriteGetPosition sprite 
    v <- fromJustTrace "Velocity var" <$> uiElementGetVar sprite varVelocity 
    let newPos = p + v * realToFrac t
        wrapedPos = Vector2 
          (if newPos^.x < 0 then newPos^.x + gw else 
            if newPos^.x >= gw then newPos^.x - gw else newPos^.x) 
          (if newPos^.y < 0 then newPos^.y + gh else 
            if newPos^.y >= gh then newPos^.y - gh else newPos^.y)
    spriteSetPosition sprite wrapedPos

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SharedPtr Application -> [SharedPtr Sprite] -> IO ()
subscribeToEvents app sprites = do 
  -- Subscribe HandleUpdate() function for processing update events
  subscribeToEvent app $ handleUpdate app sprites

-- | Handle the logic update event.
handleUpdate :: SharedPtr Application -> [SharedPtr Sprite] -> EventUpdate -> IO ()
handleUpdate app sprites e = do 
  -- Take the frame time step, which is stored as a float
  let t = e^.timeStep 

  -- Move sprites, scale movement with time step
  moveSprites app sprites t