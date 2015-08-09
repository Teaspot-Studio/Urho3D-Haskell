{-# LANGUAGE TemplateHaskell #-}
module Internal.Sample where

import Graphics.Urho3D
import Data.Word 
import Foreign
import Control.Lens 

data Sample = Sample {
  _sampleApplication :: Ptr Application 
, _sampleName :: String
, _sampleYaw :: Double
, _samplePitch :: Double 
, _sampleTouchEnabled :: Bool
, _sampleScreenJoystickIndex :: Word32
, _sampleScreenSettingsIndex :: Word32
, _samplePaused :: Bool
, _sampleLogo :: SharedSpritePtr
, _sampleScene :: SharedScenePtr
, _sampleCameraNode :: SharedNodePtr
, _sampleJoystickPatch :: String 
}
makeLenses ''Sample 