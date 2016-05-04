module Internal.Sample where

import Graphics.Urho3D
import Data.Int 
import Foreign
import Control.Lens 

data Sample = Sample {
  _sampleApplication :: SharedPtr Application
, _sampleName :: String
, _sampleYaw :: Float
, _samplePitch :: Float 
, _sampleTouchEnabled :: Bool
, _sampleScreenJoystickIndex :: Int32
, _sampleScreenSettingsIndex :: Int32
, _samplePaused :: Bool
, _sampleLogo :: Ptr Sprite
, _sampleScene :: SharedPtr Scene
, _sampleCameraNode :: SharedPtr Node
, _sampleJoystickPatch :: String 
}
makeLenses ''Sample 