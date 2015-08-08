{-# LANGUAGE TypeFamilies, RecordWildCards #-}
module Sample(
    Sample
  , newSample
  , deleteSample
  , sampleStart 
  , sampleStop 
  ) where 

import Graphics.Urho3D
import Data.Word 
import Foreign
import Data.StateVar
import Data.Maybe 

data Sample = Sample {
  sampleApplication :: Ptr Application 
, sampleName :: String
, sampleYaw :: Double
, samplePitch :: Double 
, sampleTouchEnabled :: Bool
, sampleScreenJoystickIndex :: Word32
, sampleScreenSettingsIndex :: Word32
, samplePaused :: Bool
, sampleSprite :: SharedSpritePtr
, sampleScene :: SharedScenePtr
, sampleCameraNode :: SharedNodePtr
}

newSample :: Ptr Context -> IO Sample 
newSample context = do 
  app <- newObject context 
  sprite <- newObject nullPtr 
  scene <- newObject nullPtr
  camNode <- newObject nullPtr
  return $ Sample {
    sampleApplication = app 
  , sampleYaw = 0
  , samplePitch = 0
  , sampleTouchEnabled = False
  , sampleScreenSettingsIndex = maxBound
  , sampleScreenJoystickIndex = maxBound 
  , samplePaused = False 
  , sampleSprite = sprite
  , sampleScene = scene
  , sampleCameraNode = camNode
  }

deleteSample :: Sample -> IO ()
deleteSample s = do
  deleteObject $ sampleApplication s
  deleteObject $ sampleSprite s 
  deleteObject $ sampleScene s 
  deleteObject $ sampleCameraNode s 

sampleSetup :: Sample -> IO ()
sampleSetup (Sample{..}) = do
  startupParameter sampleApplication "WindowTitle" $= sampleName

  fs <- fromJust <$> getSubsystem sampleApplication
  prefDir <- getAppPreferencesDir fs "urho3d" "logs"
  startupParameter sampleApplication "LogName" $= prefDir ++ sampleName ++ ".log"

  startupParameter sampleApplication "FullScreen" $= False 
  startupParameter sampleApplication "Headless" $= False 

sampleStart :: Sample -> IO ()
sampleStart s@(Sample{..}) = do 
  if platform == "Android" || platform == "iOS" 
  then initTouchInput s
  else do 
    is <- fromJust <$> getSubsystem sampleApplication
    jcount <- getNumJoysticks is
    when (jcount == 0) $ subscribeToEvent sampleApplication EventTouchBegin (const handleTouchBegin)

initTouchInput :: Sample -> IO ()
initTouchInput = undefined

handleTouchBegin :: IO ()
handleTouchBegin = undefined

sampleStop :: Sample -> IO ()
sampleStop (Sample{..}) = do 
  eng <- applicationEngine sampleApplication
  engineDumpResources eng True