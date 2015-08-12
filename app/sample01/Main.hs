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

-- | Helper function that prints profided message when get Nothing
fromJustTrace :: String -> Maybe a -> a 
fromJustTrace msg Nothing = error $ "fromJust: " ++ msg 
fromJustTrace _ (Just a) = a 

createText :: SharedApplicationPtr -> IO ()
createText app = do 
  -- Getting resouce cache to manipulate resource loading
  (cache :: Ptr ResourceCache) <- fromJustTrace "No ResourceCache!" <$> getSubsystem app

  -- Construct new Text object
  (helloText :: SharedTextPtr) <- newSharedObject =<< getContext app

  -- Set string to display
  textSetText helloText "Hello World from Urho3D!"

  -- Set font and text color
  (font :: Ptr Font) <- fromJustTrace "Cannot load Fonts/Anonymous Pro.ttf!" <$> cacheGetResource cache "Fonts/Anonymous Pro.ttf" True
  textSetFont helloText font 30

  -- Align Text center-screen
  uiElementSetAlignment helloText AlignmentHorizontalCenter AlignmentVerticalCenter

  -- Add Text instance to the UI root element
  (ui :: Ptr UI) <- fromJustTrace "No UI!" <$> getSubsystem app 
  root <- uiRoot ui 
  uiElementAddChild root helloText

subscribeToEvents :: SharedApplicationPtr -> IO ()
subscribeToEvents app = 
  --  Subscribe handleUpdate function for processing update events
  subscribeToEvent app handleUpdate

handleUpdate :: EventUpdate -> IO ()
handleUpdate _ = return () -- Do nothing for now, could be extended to eg. animate the display