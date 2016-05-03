module Graphics.Urho3D.Input.Internal.Input(
    Input
  , MouseMode(..)
  , TouchState(..)
  , touchedElement 
  , touchID 
  , touchPosition 
  , touchLastPosition 
  , touchDelta 
  , touchPressure
  , inputCntx
  , JoystickState(..)
  , HasJoystick(..)
  , HasJoystickID(..)
  , HasController(..)
  , HasScreenJoystick(..)
  , HasName(..)
  , HasButtons(..)
  , HasButtonPress(..)
  , HasAxes(..)
  , HasHats(..)
  , SDL.JoystickID
  , SDL.Joystick
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.UI.Internal.Element
import Graphics.Urho3D.Math.Internal.Vector2
import qualified Data.Map as Map
import Control.Lens
import Control.DeepSeq
import GHC.Generics 
import Foreign 

import qualified Data.Vector.Unboxed as VU 
import qualified SDL.Raw.Types as SDL 

data Input 

-- | Input Mouse Modes.
data MouseMode =
    MM'Absolute
  | MM'Relative
  | MM'Wrap 
  | MM'Free 
  | MM'Invalid
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData MouseMode

-- | Input state for a finger touch.
data TouchState = TouchState {
-- | Last touched UI element from screen joystick.
  _touchedElement :: !SharedWeakUIElementPtr
-- | Touch (finger) ID.
, _touchID :: !Int 
-- | Position in screen coordinates.
, _touchPosition :: !IntVector2 
-- | Last position in screen coordinates.
, _touchLastPosition :: !IntVector2
-- | Movement since last frame.
, _touchDelta :: !IntVector2
-- | Finger pressure
, _touchPressure :: !Float
} deriving Generic

makeLenses ''TouchState 

-- | Input state for a joystick.
data JoystickState = JoystickState {
  _joystickStateJoystick :: !SDL.Joystick -- ^ SDL joystick.
, _joystickStateJoystickID :: !SDL.JoystickID -- ^ SDL joystick instance ID.
, _joystickStateController :: !SDL.GameController -- ^ SDL game controller.
, _joystickStateScreenJoystick :: !(Ptr UIElement) -- ^ UI element containing the screen joystick.
, _joystickStateName :: !String -- ^ Joystick name.
, _joystickStateButtons :: !(VU.Vector Bool) -- ^ Button up/down state
, _joystickStateButtonPress :: !(VU.Vector Bool) -- ^ Button pressed on this frame
, _joystickStateAxes :: !(VU.Vector Float) -- ^ Axis poistion from -1 to 1.
, _joystickStateHats :: !(VU.Vector Int) -- ^ POV hat bits
} deriving Generic 

makeFields ''JoystickState

instance NFData TouchState where
  rnf TouchState{..} = 
    _touchedElement `seq`
    _touchID `deepseq`
    _touchPosition `deepseq`
    _touchLastPosition `deepseq`
    _touchDelta `deepseq`
    _touchPressure `deepseq` ()

inputCntx :: C.Context 
inputCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Input", [t| Input |])
    , (C.TypeName "TouchState", [t| TouchState |])
    , (C.TypeName "SDL_JoystickID", [t| SDL.JoystickID |])
    , (C.TypeName "SDL_Joystick", [t| SDL.Joystick |])
    , (C.TypeName "SDL_GameController", [t| SDL.GameController |])
    ]
  } 