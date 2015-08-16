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
  window <- initWindow app root

  -- Create and add some controls to the window
  initControls app window

  -- Create a draggable Fish 
  createDraggableFish

-- | Create and initialize a Window control.
initWindow :: SharedApplicationPtr -> Ptr UIElement -> IO SharedWindowPtr
initWindow app root = do 
  cntx <- getContext app 

  -- Create the windwo and add it to the UI's root node
  (window :: SharedWindowPtr) <- newSharedObject cntx 
  uiElementAddChild root window

  -- Set Window size and layout settings
  uiElementSetMinSize window $ IntVector2 384 192
  uiElementSetLayout window LayoutVertical 6 $ IntRect 6 6 6 6 
  uiElementSetAlignment window  AlignmentHorizontalCenter AlignmentVerticalCenter
  uiElementSetName window "Window"

  -- Create Window 'titleBar' container
  (titleBar :: SharedUIElementPtr) <- newSharedObject cntx 
  uiElementSetMinSize titleBar $ IntVector2 0 24 
  uiElementSetVerticalAlignment titleBar AlignmentTop 
  uiElementSetLayoutMode titleBar LayoutHorizontal

  -- Create the Window title Text 
  (windowTitle :: SharedTextPtr) <- newSharedObject cntx 
  uiElementSetName windowTitle "WindowTitle"
  textSetText windowTitle "Hello GUI!"

  -- Create the Window's close button
  (buttonClose :: SharedButtonPtr) <- newSharedObject cntx 
  uiElementSetName buttonClose "CloseButton"

  -- Add the controls to the title bar 
  uiElementAddChild titleBar windowTitle
  uiElementAddChild titleBar buttonClose

  -- Add the title bar to the Window
  uiElementAddChild window titleBar

  -- Apply sytles
  _ <- uiElementSetStyleAutoDefault window 
  _ <- uiElementSetStyleAutoDefault windowTitle
  uiElementSetStyleDefault buttonClose "CloseButton"

  -- Subscride to buttonClose release (following a 'press') events
  subscribeToEventSpecific app buttonClose handleClosePressed

  -- Subscride also to all UI mouse clicks just to see where we have clicked
  subscribeToEvent app handleControlClicked
  return window

-- | Create and add various common controls for demonstration purposes.
initControls :: SharedApplicationPtr -> SharedWindowPtr -> IO ()
initControls app window = do 
  cntx <- getContext app 

  -- Create a CheckBox
  (checkBox :: SharedCheckBoxPtr) <- newSharedObject cntx 
  uiElementSetName checkBox "CheckBox"

  -- Create a Button
  (button :: SharedButtonPtr) <- newSharedObject cntx 
  uiElementSetName button "Button"
  uiElementSetMinHeight button 24

  -- Create a LineEdit
  (lineEdit :: SharedLineEditPtr) <- newSharedObject cntx 
  uiElementSetName lineEdit "LineEdit"
  uiElementSetMinHeight lineEdit 24 

  -- Add controls to Window 
  uiElementAddChild window checkBox
  uiElementAddChild window button 
  uiElementAddChild window lineEdit

  -- Apply previously set default style
  _ <- uiElementSetStyleAutoDefault checkBox
  _ <- uiElementSetStyleAutoDefault button
  _ <- uiElementSetStyleAutoDefault lineEdit
  return ()

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
handleControlClicked :: EventUIMouseClick -> IO ()
handleControlClicked = undefined

-- | Handle close button pressed and released.
handleClosePressed :: EventReleased -> IO ()
handleClosePressed = undefined

