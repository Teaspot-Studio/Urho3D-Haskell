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
 This first example, maintaining tradition, prints a "Hello World" message.
 Furthermore it shows:
     - Using the Sample / Application classes, which initialize the Urho3D engine and run the main loop
     - Adding a Text element to the graphical user interface
     - Subscribing to and handling of update events
-}
module Main where

import Control.Lens hiding (Context)
import Data.IORef
import Foreign

import Graphics.Urho3D
import Sample

main :: IO ()
main = withObject () $ \cntx -> do 
  newSample cntx "HelloWorld" joysticPatch customStart >>= runSample

-- | Setup after engine initialization and before running the main loop.
customStart :: SampleRef -> IO ()
customStart sr = do 
  s <- readIORef sr
  let app = s^.sampleApplication

  -- Create "Hello World" Text
  createText app 

  {-
    Finally subscribe to the update event. Note that by subscribing events at this point we have already missed some events
    like the ScreenMode event sent by the Graphics subsystem when opening the application window. To catch those as well we
    could subscribe in the constructor instead.
  -}
  subscribeToEvents app 

-- | Construct a new Text instance, containing the 'Hello World' String, and add it to the UI root element.
createText :: SharedPtr Application -> IO ()
createText app = do 
  -- Getting resouce cache to manipulate resource loading
  (cache :: Ptr ResourceCache) <- fromJustTrace "No ResourceCache!" <$> getSubsystem app

  -- Construct new Text object
  (helloText :: SharedPtr Text) <- newSharedObject =<< getContext app

  -- Set string to display
  textSetText helloText "Hello World from Urho3D!"

  -- Set font and text color
  (font :: Ptr Font) <- fromJustTrace "Cannot load Fonts/Anonymous Pro.ttf!" <$> cacheGetResource cache "Fonts/Anonymous Pro.ttf" True
  textSetFont helloText font 30
  uiElementSetColor helloText $ rgb 0 1 0

  -- Align Text center-screen
  uiElementSetAlignment helloText AlignmentHorizontalCenter AlignmentVerticalCenter

  -- Add Text instance to the UI root element
  (ui :: Ptr UI) <- fromJustTrace "No UI!" <$> getSubsystem app 
  rootElem <- uiRoot ui 
  uiElementAddChild rootElem helloText

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SharedPtr Application -> IO ()
subscribeToEvents app = 
  --  Subscribe handleUpdate function for processing update events
  subscribeToEvent app handleUpdate

-- | Handle the logic update event.
handleUpdate :: EventUpdate -> IO ()
handleUpdate _ = return () -- Do nothing for now, could be extended to eg. animate the display