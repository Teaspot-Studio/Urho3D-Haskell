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
  createDraggableFish app root

-- | Create and initialize a Window control.
initWindow :: SharedApplicationPtr -> Ptr UIElement -> IO (Ptr Window)
initWindow app root = do 
  cntx <- getContext app 

  -- Create the windwo and add it to the UI's root node
  (window :: Ptr Window) <- newObject cntx 
  uiElementAddChild root window

  -- Set Window size and layout settings
  uiElementSetMinSize window $ IntVector2 384 192
  uiElementSetLayout window LayoutVertical 6 $ IntRect 6 6 6 6 
  uiElementSetAlignment window  AlignmentHorizontalCenter AlignmentVerticalCenter
  uiElementSetName window "Window"

  -- Create Window 'titleBar' container
  (titleBar :: Ptr UIElement) <- newObject cntx 
  uiElementSetMinSize titleBar $ IntVector2 0 24 
  uiElementSetVerticalAlignment titleBar AlignmentTop 
  uiElementSetLayoutMode titleBar LayoutHorizontal

  -- Create the Window title Text 
  (windowTitle :: Ptr Text) <- newObject cntx 
  uiElementSetName windowTitle "WindowTitle"
  textSetText windowTitle "Hello GUI!"

  -- Create the Window's close button
  (buttonClose :: Ptr Button) <- newObject cntx 
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
initControls :: SharedApplicationPtr -> Ptr Window -> IO ()
initControls app window = do 
  cntx <- getContext app 

  -- Create a CheckBox
  (checkBox :: Ptr CheckBox) <- newObject cntx 
  uiElementSetName checkBox "CheckBox"

  -- Create a Button
  (button :: Ptr Button) <- newObject cntx 
  uiElementSetName button "Button"
  uiElementSetMinHeight button 24

  -- Create a LineEdit
  (lineEdit :: Ptr LineEdit) <- newObject cntx 
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
createDraggableFish :: SharedApplicationPtr -> Ptr UIElement-> IO ()
createDraggableFish app root = do
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app 
  (graphics :: Ptr Graphics) <- fromJustTrace "Graphics" <$> getSubsystem app 
  cntx <- getContext app 

  -- Create a draggable Fish button
  (draggableFish :: Ptr Button) <- newObject cntx 
  (tex :: Ptr Texture) <-fromJustTrace "UrhoDecal.dds" <$> cacheGetResource cache "Textures/UrhoDecal.dds" True
  borderImageSetTexture draggableFish tex
  borderImageSetBlendMode draggableFish BlendAdd 
  uiElementSetSize draggableFish $ IntVector2 128 128
  gwidth <- graphicsGetWidth graphics 
  width <- uiElementGetWidth draggableFish
  uiElementSetPosition draggableFish $ IntVector2 ((gwidth - width) `div` 2) 200
  uiElementSetName draggableFish "Fish"
  uiElementAddChild root draggableFish

  -- Add a tooltip to Fish button
  (toolTip :: Ptr ToolTip) <- newObject cntx 
  uiElementAddChild draggableFish toolTip
  uiElementSetPosition toolTip $ IntVector2 (width + 5) (width `div` 2)
  
  (textHolder :: Ptr BorderImage) <- newObject cntx 
  uiElementAddChild toolTip textHolder
  uiElementSetStyleDefault textHolder "ToolTipBorderImage"

  (toolTipText :: Ptr Text) <- newObject cntx
  uiElementAddChild textHolder toolTipText
  uiElementSetStyleDefault toolTipText "ToolTipText"
  textSetText toolTipText "Please drag me!"

  -- Subscribe draggableFish to Drag Events (in order to make it draggable)
  -- See "Event list" in documentation's Main Page for reference on available Events and their eventData
  subscribeToEventSpecific app draggableFish handleDragBegin
  subscribeToEventSpecific app draggableFish handleDragMove
  subscribeToEventSpecific app draggableFish handleDragEnd

-- | Handle drag begin for the fish button.
handleDragBegin :: EventDragBegin -> IO ()
handleDragBegin = undefined

-- | Handle drag move for the fish button.
handleDragMove :: EventDragMove -> IO ()
handleDragMove = undefined

-- | Handle drag end for the fish button.
handleDragEnd :: EventDragEnd -> IO ()
handleDragEnd =  undefined

-- | Handle any UI control being clicked.
handleControlClicked :: EventUIMouseClick -> IO ()
handleControlClicked = undefined

-- | Handle close button pressed and released.
handleClosePressed :: EventReleased -> IO ()
handleClosePressed = undefined

