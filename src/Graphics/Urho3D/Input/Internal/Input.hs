{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Input.Internal.Input(
    Input
  , TouchState(..)
  , touchedElement 
  , touchID 
  , touchPosition 
  , touchLastPosition 
  , touchDelta 
  , touchPressure
  , JoystickID
  , inputCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.UI.Internal.Element
import Graphics.Urho3D.Math.Internal.Vector2
import qualified Data.Map as Map
import Data.Int 
import Control.Lens 
import Foreign 

data Input 
type JoystickID = Int32

-- | Input state for a finger touch.
data TouchState = TouchState {
-- | Last touched UI element from screen joystick.
  _touchedElement :: Maybe (Ptr UIElement)
-- | Touch (finger) ID.
, _touchID :: Int 
-- | Position in screen coordinates.
, _touchPosition :: IntVector2 
-- | Last position in screen coordinates.
, _touchLastPosition :: IntVector2
-- | Movement since last frame.
, _touchDelta :: IntVector2
-- | Finger pressure
, _touchPressure :: Float
}
makeLenses ''TouchState 

inputCntx :: C.Context 
inputCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Input", [t| Input |])
    , (C.TypeName "TouchState", [t| TouchState |])
    , (C.TypeName "SDL_JoystickID", [t| JoystickID |])
    ]
  } 