{-# LANGUAGE TemplateHaskell #-}
module Internal.Sample where

import Graphics.Urho3D
import Data.Int 
import Foreign
import Control.Lens 
import Data.IORef

data Sample = Sample {
  _sampleApplication :: Ptr Application 
, _sampleName :: String
, _sampleYaw :: Double
, _samplePitch :: Double 
, _sampleTouchEnabled :: Bool
, _sampleScreenJoystickIndex :: Int32
, _sampleScreenSettingsIndex :: IORef Int32
, _samplePaused :: IORef Bool
, _sampleLogo :: SharedSpritePtr
, _sampleScene :: SharedScenePtr
, _sampleCameraNode :: SharedNodePtr
, _sampleJoystickPatch :: String 
}
makeLenses ''Sample 