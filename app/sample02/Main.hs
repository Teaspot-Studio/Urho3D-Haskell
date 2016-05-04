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
module Main where

import qualified Data.Text as T
import Control.Lens hiding (Context, element)
import Data.IORef
import Data.Monoid
import Foreign
import Graphics.Urho3D
import Sample

main :: IO ()
main = withObject () $ \cntx -> do 
  newSample cntx "HelloGUI" joysticPatch customStart >>= runSample

-- | Setup after engine initialization and before running the main loop.
customStart :: SampleRef -> IO ()
customStart sr = do 
  s <- readIORef sr
  let app = s ^. sampleApplication 

  -- Enable OS cursor
  (input :: Ptr Input) <- fromJustTrace "Input system" <$> getSubsystem app 
  inputSetMouseVisible input True False

  -- Load XML file containing default UI style sheet 
  (cache :: Ptr ResourceCache) <- fromJustTrace "Resource Cache" <$> getSubsystem app 
  (style :: Ptr XMLFile) <- fromJustTrace "Failed to load UI/DefaultStyle.xml" <$> cacheGetResource cache "UI/DefaultStyle.xml" True

  (ui :: Ptr UI) <- fromJustTrace "UI system" <$> getSubsystem app 
  (roote :: Ptr UIElement) <- uiRoot ui 
  uiElementSetDefaultStyle roote style 

  -- Initialize window
  window <- initWindow app roote

  -- Create and add some controls to the window
  initControls app window

  -- Create a draggable Fish
  createDraggableFish app roote

-- | Create and initialize a Window control.
initWindow :: SharedPtr Application-> Ptr UIElement -> IO (Ptr Window)
initWindow app roote = do 
  cntx <- getContext app 

  -- Create the windwo and add it to the UI's root node
  (window :: Ptr Window) <- newObject cntx 
  uiElementAddChild roote window

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
  _ <- uiElementSetStyleDefault buttonClose "CloseButton"

  -- Subscride to buttonClose release (following a 'press') events
  subscribeToEventSpecific app buttonClose $ handleClosePressed app

  -- Subscride also to all UI mouse clicks just to see where we have clicked
  subscribeToEvent app $ handleControlClicked window
  return window

-- | Create and add various common controls for demonstration purposes.
initControls :: SharedPtr Application -> Ptr Window -> IO ()
initControls app window = do 
  cntx <- getContext app 

  -- Create a CheckBox
  (checkBox :: Ptr CheckBox) <- newObject cntx 
  uiElementSetName checkBox "CheckBox"

  -- Create a Button
  (btn :: Ptr Button) <- newObject cntx 
  uiElementSetName btn "Button"
  uiElementSetMinHeight btn 24

  -- Create a LineEdit
  (lineEdit :: Ptr LineEdit) <- newObject cntx 
  uiElementSetName lineEdit "LineEdit"
  uiElementSetMinHeight lineEdit 24 

  -- Add controls to Window 
  uiElementAddChild window checkBox
  uiElementAddChild window btn 
  uiElementAddChild window lineEdit

  -- Apply previously set default style
  _ <- uiElementSetStyleAutoDefault checkBox
  _ <- uiElementSetStyleAutoDefault btn
  _ <- uiElementSetStyleAutoDefault lineEdit
  return ()

-- | Create a draggable fish button.
createDraggableFish :: SharedPtr Application -> Ptr UIElement-> IO ()
createDraggableFish app roote = do
  (cache :: Ptr ResourceCache) <- fromJustTrace "ResourceCache" <$> getSubsystem app 
  (graphics :: Ptr Graphics) <- fromJustTrace "Graphics" <$> getSubsystem app 
  cntx <- getContext app 

  -- Create a draggable Fish button
  (draggableFish :: Ptr Button) <- newObject cntx 
  (tex :: Ptr Texture2D) <- fromJustTrace "UrhoDecal.dds" <$> cacheGetResource cache "Textures/UrhoDecal.dds" True
  borderImageSetTexture draggableFish tex
  borderImageSetBlendMode draggableFish BlendAdd 
  uiElementSetSize draggableFish $ IntVector2 128 128
  gw <- graphicsGetWidth graphics 
  fw <- uiElementGetWidth draggableFish
  uiElementSetPosition draggableFish $ IntVector2 ((gw - fw) `div` 2) 200
  uiElementSetName draggableFish "Fish"
  uiElementAddChild roote draggableFish

  -- Add a tooltip to Fish button
  (toolTip :: Ptr ToolTip) <- newObject cntx 
  uiElementAddChild draggableFish toolTip
  uiElementSetPosition toolTip $ IntVector2 (fw + 5) (fw `div` 2)
  
  (textHolder :: Ptr BorderImage) <- newObject cntx 
  uiElementAddChild toolTip textHolder
  _ <- uiElementSetStyleDefault textHolder "ToolTipBorderImage"

  (toolTipText :: Ptr Text) <- newObject cntx
  uiElementAddChild textHolder toolTipText
  _ <- uiElementSetStyleDefault toolTipText "ToolTipText"
  textSetText toolTipText "Please drag me!"

  -- Subscribe draggableFish to Drag Events (in order to make it draggable)
  -- See "Event list" in documentation's Main Page for reference on available Events and their eventData
  dragBeginPosRef <- newIORef 0
  subscribeToEventSpecific app draggableFish $ handleDragBegin dragBeginPosRef
  subscribeToEventSpecific app draggableFish $ handleDragMove dragBeginPosRef
  subscribeToEventSpecific app draggableFish handleDragEnd

-- | Handle drag begin for the fish button.
handleDragBegin :: IORef IntVector2 -> EventDragBegin -> IO ()
handleDragBegin dragBeginPosRef e = do
  -- Get UIElement relative position where input (touch or click) occured (top-left = IntVector2(0,0))
  writeIORef dragBeginPosRef $ IntVector2 (e^.elementX) (e^.elementY)

-- | Handle drag move for the fish button.
handleDragMove :: IORef IntVector2 -> EventDragMove -> IO ()
handleDragMove dragBeginPosRef e = do
  dragBeginPosition <- readIORef dragBeginPosRef
  let dragCurrentPosition = IntVector2 (e^.x) (e^.y)
  uiElementSetPosition (e^.element) (dragCurrentPosition - dragBeginPosition)

-- | Handle drag end for the fish button.
handleDragEnd :: EventDragEnd -> IO ()
handleDragEnd _ = return ()

-- | Handle close button pressed and released.
handleClosePressed :: SharedPtr Application -> EventReleased -> IO ()
handleClosePressed app _ = do 
  engine <- applicationEngine app 
  engineExit engine

-- | Handle any UI control being clicked.
handleControlClicked :: Ptr Window -> EventUIMouseClick -> IO ()
handleControlClicked window e = do 
  -- Get the Text control acting as the Window's title
  (windowTitle :: Ptr Text) <- fromJustTrace "WindowTitle" <$> uiElementGetChildByName window "WindowTitle" True 
  
  -- Get control that was clicked
  let clicked = e^.element 

  -- Get the name of the control that was clicked
  n <- maybeNull (return "...?") uiElementGetName clicked 

  -- Update the Window's title text
  textSetText windowTitle $ "Hello " <> T.pack n <> "!"
