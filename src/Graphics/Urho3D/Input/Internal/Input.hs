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
import Control.DeepSeq
import GHC.Generics 

data Input 
type JoystickID = Int32

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
    , (C.TypeName "SDL_JoystickID", [t| JoystickID |])
    ]
  } 