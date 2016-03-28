module Internal.Sample where

import Graphics.Urho3D
import Data.Int 
import Foreign
import Control.Lens 
import Data.IORef

data Sample = Sample {
  _sampleApplication :: SharedApplicationPtr
, _sampleName :: String
, _sampleYaw :: Float
, _samplePitch :: Float 
, _sampleTouchEnabled :: Bool
, _sampleScreenJoystickIndex :: Int32
, _sampleScreenSettingsIndex :: Int32
, _samplePaused :: Bool
, _sampleLogo :: Ptr Sprite
, _sampleScene :: SharedScenePtr
, _sampleCameraNode :: SharedNodePtr
, _sampleJoystickPatch :: String 
}
makeLenses ''Sample 