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
{- A simple 'HelloWorld' GUI created purely from code.
This sample demonstrates:
    - Creation of controls and building a UI hierarchy
    - Loading UI style from XML and applying it to controls
    - Handling of global and per-control events
For more advanced users (beginners can skip this section):
    - Dragging UIElements
    - Displaying tooltips
    - Accessing available Events data (eventData)
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.RawString.QQ
import Control.Lens hiding (Context)
import Data.IORef
import Foreign

import Graphics.Urho3D
import Sample

-- | Return XML patch instructions for screen joystick layout for a specific sample app, if any.
joysticPatch :: String 
joysticPatch = [r|
<patch>
    <add sel=\"/element/element[./attribute[@name='Name' and @value='Hat0']]\">
        <attribute name=\"Is Visible\" value=\"false\" />
    </add>
</patch>
|]

{- TODO:
    /// The Window.
    SharedPtr<Window> window_;
    /// The UI's root UIElement.
    SharedPtr<UIElement> uiRoot_;
    /// Remembered drag begin position.
    IntVector2 dragBeginPosition_;
-}

main :: IO ()
main = withObject () $ \cntx -> do 
  newSample cntx "HelloWorld" joysticPatch customStart >>= runSample

-- | Helper function that prints profided message when get Nothing
fromJustTrace :: String -> Maybe a -> a 
fromJustTrace msg Nothing = error $ "fromJust: " ++ msg 
fromJustTrace _ (Just a) = a 

-- | Setup after engine initialization and before running the main loop.
customStart :: SampleRef -> IO ()
customStart sr = do 
  s <- readIORef sr
  let app = s ^. sampleApplication 

  -- Enable OS cursor
  (input :: Ptr Input) <- fromJustTrace "Input system" <$> getSubsystem app 
  inputSetMouseVisible input True 

  -- Load XML file containing default UI style sheet 
  (cache :: Ptr ResourceCache) <- fromJustTrace "Resource Cache" <$> getSubsystem app 
  (style :: Ptr XMLFile) <- fromJustTrace "Failed to load UI/DefaultStyle.xml" <$> cacheGetResource cache "UI/DefaultStyle.xml" True

  (ui :: Ptr UI) <- fromJustTrace "UI system" <$> getSubsystem app 
  (root :: Ptr UIElement) <- uiRoot ui 
  uiElementSetDefaultStyle root style 

  -- Initialize window
  initWindow

  -- Create and add some controls to the window
  initControls 

  -- Create a draggable Fish 
  createDraggableFish

-- | Create and initialize a Window control.
initWindow :: IO ()
initWindow = undefined

-- | Create and add various common controls for demonstration purposes.
initControls :: IO ()
initControls = undefined

-- | Create a draggable fish button.
createDraggableFish :: IO ()
createDraggableFish = undefined

-- | Handle drag begin for the fish button.
handleDragBegin :: IO ()
handleDragBegin = undefined

-- | Handle drag move for the fish button.
handleDragMove :: IO ()
handleDragMove = undefined

-- | Handle drag end for the fish button.
handleDragEnd :: IO ()
handleDragEnd =  undefined

-- | Handle any UI control being clicked.
handleControlClicked :: IO ()
handleControlClicked = undefined

-- | Handle close button pressed and released.
handleClosePressed :: IO ()
handleClosePressed = undefined

