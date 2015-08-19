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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Control.Lens hiding (Context, element)
import Control.Monad 
import Data.IORef
import Data.Monoid
import Foreign
import Graphics.Urho3D
import Sample
import Text.RawString.QQ

-- | Return XML patch instructions for screen joystick layout for a specific sample app, if any.
joysticPatch :: String 
joysticPatch = [r|
<patch>
    <add sel=\"/element/element[./attribute[@name='Name' and @value='Hat0']]\">
        <attribute name=\"Is Visible\" value=\"false\" />
    </add>
</patch>
|]

main :: IO ()
main = withObject () $ \cntx -> do 
  newSample cntx "HelloWorld" joysticPatch customStart >>= runSample

-- Number of sprites to draw
numSprites :: Int 
numSprites = 100 

-- Custom vairable identifier for storing velocity within the UI element
varVelocity :: String 
varVelocity = "Velocity"

-- | Helper function that prints profided message when get Nothing
fromJustTrace :: String -> Maybe a -> a 
fromJustTrace msg Nothing = error $ "fromJust: " ++ msg 
fromJustTrace _ (Just a) = a 

-- | Setup after engine initialization and before running the main loop.
customStart :: SampleRef -> IO ()
customStart sr = do 
  s <- readIORef sr 
  let app = s^.sampleApplication

  -- Create the sprites to the user interface
  srpites <- createSprites app
  -- Hook up to the frame update events
  subscribeToEvents

-- | Construct the sprites.
createSprites :: SharedApplicationPtr -> IO [SharedSpritePtr]
createSprites app = do 
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app 
  (graphics :: Ptr Graphics) <- fromJustTrace "Graphics" <$> getSubsystem app 
  (ui :: Ptr UI) <- fromJustTrace "UI" <$> getSubsystem app 

  -- Get rendering window size as floats
  width <- fromIntegral <$> graphicsGetWidth graphics
  height <- fromIntegral <$> graphicsGetHeight graphics

  -- Get the Urho3D fish texture
  (decalTex :: Ptr Texture2D) <- fromJustTrace "UrhoDecal.dds" <$> cacheGetResource cache "Texture/UrhoDecal.dds" True 

  root <- uiRoot ui 
  forM [1 .. numSprites] $ const $ do 
    -- Create a new sprite, set it to use the texture
    (sprite :: SharedSpritePtr) <- newSharedObject =<< getContext app
    spriteSetTexture sprite decalTex

    -- The UI root element is as big as the rendering window, set random position within it 
    [r1, r2] <- replicateM 2 random
    spriteSetPosition sprite $ Vector2 (r1 * width) (r2 * height) 

    -- Set sprite size & hotspot in its center
    uiElementSetSize sprite 128
    spriteSetHotSpot sprite 64

    -- Set random rotation in degrees and random scale 
    spriteSetRotation sprite =<< (360 *) <$> random
    spriteSetScale sprite =<< (0.5 +) <$> randomUp 1.0

    -- Set random color and additive blending mode
    [cr1, cr2, cr3] <- replicateM 3 $ randomUp 0.5
    uiElementSetColor sprite $ rgb (cr1 + 0.5) (cr2 + 0.5) (cr3 + 0.5)
    spriteSetBlendMode sprite BlendAdd 

    -- Add as a child of the root UI element
    uiElementAddChild root sprite 

    -- Store sprite's velocity as a custom variable 
    [vr1, vr2] <- replicateM 2 random
    uiElementSetVar sprite varVelocity $ Vector2 (vr1 - 100) (vr2 - 100)

    -- Store sprites to our own container for easy movement update iteration
    return sprite 

-- | Move the sprites using the delta time step given.
moveSprites :: Float -> IO ()
moveSprites = undefined

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: IO ()
subscribeToEvents = undefined

-- | Handle the logic update event.
handleUpdate :: EventUpdate -> IO ()
handleUpdate = undefined