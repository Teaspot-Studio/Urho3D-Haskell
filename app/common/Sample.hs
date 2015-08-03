module Sample(
    Sample
  , newSample
  ) where 

import Graphics.Urho3D
import Data.Word 
import Foreign

data Sample = Sample {
  sampleApplication :: Ptr Application 
, sampleYaw :: Double
, samplePitch :: Double 
, sampleTouchEnabled :: Bool
, sampleScreenJoystickIndex :: Word32
, sampleScreenSettingsIndex :: Word32
, samplePaused :: Bool
}

newSample :: Ptr Context -> IO Sample 
newSample context = do 
  app <- newApplication context 
  return $ Sample {
    sampleApplication = app 
  , sampleYaw = 0
  , samplePitch = 0
  , sampleTouchEnabled = False
  , sampleScreenSettingsIndex = maxBound
  , sampleScreenJoystickIndex = maxBound 
  , samplePaused = False 
  }