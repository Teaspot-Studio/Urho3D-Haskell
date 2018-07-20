module Graphics.Urho3D.Input.Events(
    MouseButtonDown(..)
  , MouseButtonUp(..)
  , MouseMove(..)
  , MouseWheel(..)
  , EventKeyDown(..)
  , EventKeyUp(..)
  , EventTextInput(..)
  , EventTextEditing(..)
  , EventJoystickConnected(..)
  , EventJoystickDisconnected(..)
  , EventJoystickButtonDown(..)
  , EventJoystickButtonUp(..)
  , EventJoystickAxisMove(..)
  , EventJoystickHatMove(..)
  , EventTouchBegin(..)
  , EventTouchEnd(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Graphics.Urho3D.Container.FlagSet
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Input.InputConstants
import Graphics.Urho3D.Math.StringHash

C.context (C.cppCtx <> stringHashContext)
C.include "<Urho3D/Input/InputEvents.h>"
C.using "namespace Urho3D"

-- | Fires when user press down keyboard key
data MouseButtonDown = MouseButtonDown {
  mouseButtonDownButton     :: !MouseButton
, mouseButtonDownButtons    :: !MouseButtonFlags
, mouseButtonDownQualifiers :: !QualifierFlags
} deriving (Show)

instance Event MouseButtonDown where
  eventID _ = [C.pure| const StringHash* {&E_MOUSEBUTTONDOWN} |]
  loadEventData vmap = do
    pbutton :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseButtonDown::P_BUTTON} |]
    pbuttons :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseButtonDown::P_BUTTONS} |]
    qualifiers :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseButtonDown::P_QUALIFIERS} |]
    return $ MouseButtonDown {
        mouseButtonDownButton = maybe MouseButtonNone toEnum pbutton
      , mouseButtonDownButtons = FlagSet $ maybe 0 fromIntegral pbuttons
      , mouseButtonDownQualifiers = FlagSet $ maybe 0 fromIntegral qualifiers
      }

-- | Fires when user press down keyboard key
data MouseButtonUp = MouseButtonUp {
  mouseButtonUpButton     :: !MouseButton
, mouseButtonUpButtons    :: !MouseButtonFlags
, mouseButtonUpQualifiers :: !QualifierFlags
} deriving (Show)

instance Event MouseButtonUp where
  eventID _ = [C.pure| const StringHash* {&E_MOUSEBUTTONUP} |]
  loadEventData vmap = do
    pbutton :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseButtonUp::P_BUTTON} |]
    pbuttons :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseButtonUp::P_BUTTONS} |]
    qualifiers :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseButtonUp::P_QUALIFIERS} |]
    return $ MouseButtonUp {
        mouseButtonUpButton = maybe MouseButtonNone toEnum pbutton
      , mouseButtonUpButtons = FlagSet $ maybe 0 fromIntegral pbuttons
      , mouseButtonUpQualifiers = FlagSet $ maybe 0 fromIntegral qualifiers
      }

-- | Fires when user press down keyboard key
data MouseMove = MouseMove {
  mouseMoveX          :: !Int
, mouseMoveY          :: !Int
, mouseMoveDX         :: !Int
, mouseMoveDY         :: !Int
, mouseMoveButtons    :: !MouseButtonFlags
, mouseMoveQualifiers :: !QualifierFlags
} deriving (Show)

instance Event MouseMove where
  eventID _ = [C.pure| const StringHash* {&E_MOUSEMOVE} |]
  loadEventData vmap = do
    px :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseMove::P_X} |]
    py :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseMove::P_Y} |]
    pdx :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseMove::P_DX} |]
    pdy :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseMove::P_DY} |]
    pbuttons :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseMove::P_BUTTONS} |]
    qualifiers :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseMove::P_QUALIFIERS} |]
    return $ MouseMove {
        mouseMoveX = fromMaybe 0 px
      , mouseMoveY = fromMaybe 0 py
      , mouseMoveDX = fromMaybe 0 pdx
      , mouseMoveDY = fromMaybe 0 pdy
      , mouseMoveButtons = FlagSet $ maybe 0 fromIntegral pbuttons
      , mouseMoveQualifiers = FlagSet $ maybe 0 fromIntegral qualifiers
      }

-- | Fires when user press down keyboard key
data MouseWheel = MouseWheel {
  mouseWheelWheel      :: !Int
, mouseWheelButtons    :: !MouseButtonFlags
, mouseWheelQualifiers :: !QualifierFlags
} deriving (Show)

instance Event MouseWheel where
  eventID _ = [C.pure| const StringHash* {&E_MOUSEMOVE} |]
  loadEventData vmap = do
    pwheel :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseWheel::P_WHEEL} |]
    pbuttons :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseWheel::P_BUTTONS} |]
    pqualifiers :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&MouseWheel::P_QUALIFIERS} |]
    return $ MouseWheel {
        mouseWheelWheel = fromMaybe 0 pwheel
      , mouseWheelButtons = FlagSet $ maybe 0 fromIntegral pbuttons
      , mouseWheelQualifiers = FlagSet $ maybe 0 fromIntegral pqualifiers
      }

-- | Fires when user press down keyboard key
data EventKeyDown = EventKeyDown {
    pressKey        :: !Key
  , pressScancode   :: !Scancode
  , pressButtons    :: !MouseButtonFlags
  , pressQualifiers :: !QualifierFlags
  , pressRepeat     :: !Bool
  } deriving (Show)

instance Event EventKeyDown where
  eventID _ = [C.pure| const StringHash* {&E_KEYDOWN} |]
  loadEventData vmap = do
    pkey :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_KEY} |]
    pscan :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_SCANCODE} |]
    pbuttons :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_BUTTONS} |]
    pqualifiers :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_QUALIFIERS} |]
    prepeat :: Maybe Bool <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_REPEAT} |]
    return $ EventKeyDown {
      pressKey = maybe KeyUnknown toEnum pkey
    , pressScancode = maybe ScancodeUnknown toEnum pscan
    , pressButtons = FlagSet $ maybe 0 fromIntegral pbuttons
    , pressQualifiers = FlagSet $ maybe 0 fromIntegral pqualifiers
    , pressRepeat = fromMaybe False prepeat
    }

-- | Fires when user release keyboard key
data EventKeyUp = EventKeyUp {
  upKey        :: !Key
, upScancode   :: !Scancode
, upButtons    :: !MouseButtonFlags
, upQualifiers :: !QualifierFlags
} deriving (Show)

instance Event EventKeyUp where
  eventID _ = [C.pure| const StringHash* {&E_KEYUP} |]
  loadEventData vmap = do
    pkey :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&KeyUp::P_KEY} |]
    pscan :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&KeyUp::P_SCANCODE} |]
    pbuttons :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&KeyUp::P_BUTTONS} |]
    pqualifiers :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&KeyUp::P_QUALIFIERS} |]
    return $ EventKeyUp {
        upKey = maybe KeyUnknown toEnum pkey
      , upScancode = maybe ScancodeUnknown toEnum pscan
      , upButtons = FlagSet $ maybe 0 fromIntegral pbuttons
      , upQualifiers = FlagSet $ maybe 0 fromIntegral pqualifiers
      }

-- | Text input event
data EventTextInput = EventTextInput {
  eventTextInputText :: !Text
} deriving (Show)

instance Event EventTextInput where
  eventID _ = [C.pure| const StringHash* {&E_TEXTINPUT} |]
  loadEventData vmap = do
    txt :: Maybe Text <- variantMapGet' vmap [C.pure| const StringHash* {&TextInput::P_TEXT} |]
    return $ EventTextInput {
        eventTextInputText = fromMaybe "" txt
      }

-- | Text editing event.
data EventTextEditing = EventTextEditing {
  eventTextEditingCompositon      :: !Text
, eventTextEditingCursor          :: !Int
, eventTextEditingSelectionLength :: !Int
} deriving (Show)

instance Event EventTextEditing where
  eventID _ = [C.pure| const StringHash* {&E_TEXTEDITING} |]
  loadEventData vmap = do
    txt :: Maybe Text <- variantMapGet' vmap [C.pure| const StringHash* {&TextEditing::P_COMPOSITION} |]
    cursor :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TextEditing::P_CURSOR} |]
    sel :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TextEditing::P_SELECTION_LENGTH} |]
    return $ EventTextEditing {
        eventTextEditingCompositon = fromMaybe "" txt
      , eventTextEditingCursor = fromMaybe 0 cursor
      , eventTextEditingSelectionLength = fromMaybe 0 sel
      }

-- | Joystick connected.
data EventJoystickConnected = EventJoystickConnected {
  eventJoystickConnectedId :: !Int
} deriving (Show)

instance Event EventJoystickConnected where
  eventID _ = [C.pure| const StringHash* {&E_JOYSTICKCONNECTED} |]
  loadEventData vmap = do
    jid :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickConnected::P_JOYSTICKID} |]
    return $ EventJoystickConnected {
        eventJoystickConnectedId = fromMaybe 0 jid
      }

-- | Joystick disconnected.
data EventJoystickDisconnected = EventJoystickDisconnected {
  eventJoystickDisconnectedId :: !Int
} deriving (Show)

instance Event EventJoystickDisconnected where
  eventID _ = [C.pure| const StringHash* {&E_JOYSTICKDISCONNECTED} |]
  loadEventData vmap = do
    jid :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickDisconnected::P_JOYSTICKID} |]
    return $ EventJoystickDisconnected {
        eventJoystickDisconnectedId = fromMaybe 0 jid
      }

-- | Joystick button pressed.
data EventJoystickButtonDown = EventJoystickButtonDown {
  eventJoystickButtonDownId  :: !Int
, eventJoystickButtonDownBtn :: !ControllerButton
} deriving (Show)

instance Event EventJoystickButtonDown where
  eventID _ = [C.pure| const StringHash* {&E_JOYSTICKBUTTONDOWN} |]
  loadEventData vmap = do
    jid :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickButtonDown::P_JOYSTICKID} |]
    btn :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickButtonDown::P_BUTTON} |]
    return $ EventJoystickButtonDown {
        eventJoystickButtonDownId = fromMaybe 0 jid
      , eventJoystickButtonDownBtn = maybe ControllerButtonA toEnum btn
      }

-- | Joystick button released.
data EventJoystickButtonUp = EventJoystickButtonUp {
  eventJoystickButtonUpId  :: !Int
, eventJoystickButtonUpBtn :: !ControllerButton
} deriving (Show)

instance Event EventJoystickButtonUp where
  eventID _ = [C.pure| const StringHash* {&E_JOYSTICKBUTTONUP} |]
  loadEventData vmap = do
    jid :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickButtonUp::P_JOYSTICKID} |]
    btn :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickButtonUp::P_BUTTON} |]
    return $ EventJoystickButtonUp {
        eventJoystickButtonUpId = fromMaybe 0 jid
      , eventJoystickButtonUpBtn = maybe ControllerButtonA toEnum btn
      }

-- | Joystick axis moved.
data EventJoystickAxisMove = EventJoystickAxisMove {
  eventJoystickAxisMoveId       :: !Int
, eventJoystickAxisMoveAxis     :: !ControllerAxis
, eventJoystickAxisMovePosition :: !Float
} deriving (Show)

instance Event EventJoystickAxisMove where
  eventID _ = [C.pure| const StringHash* {&E_JOYSTICKAXISMOVE} |]
  loadEventData vmap = do
    jid :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickAxisMove::P_JOYSTICKID} |]
    axis :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickAxisMove::P_AXIS} |]
    pos :: Maybe Float <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickAxisMove::P_POSITION} |]
    return $ EventJoystickAxisMove {
        eventJoystickAxisMoveId = fromMaybe 0 jid
      , eventJoystickAxisMoveAxis = maybe ControllerAxisLeftX toEnum axis
      , eventJoystickAxisMovePosition = fromMaybe 0 pos
      }

-- | Joystick POV hat moved.
data EventJoystickHatMove = EventJoystickHatMove {
  eventJoystickHatMoveId       :: !Int
, eventJoystickHatMoveHat      :: !HatPosition
, eventJoystickHatMovePosition :: !Int
} deriving (Show)

instance Event EventJoystickHatMove where
  eventID _ = [C.pure| const StringHash* {&E_JOYSTICKHATMOVE} |]
  loadEventData vmap = do
    jid :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickHatMove::P_JOYSTICKID} |]
    hat :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickHatMove::P_HAT} |]
    pos :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&JoystickHatMove::P_POSITION} |]
    return $ EventJoystickHatMove {
        eventJoystickHatMoveId = fromMaybe 0 jid
      , eventJoystickHatMoveHat = maybe HatCenter toEnum hat
      , eventJoystickHatMovePosition = fromMaybe 0 pos
      }

-- | Fires when user touches the screen
data EventTouchBegin = EventTouchBegin {
  eventTouchId :: !Int
, eventTouchX :: !Int
, eventTouchY :: !Int
, eventTouchPressure :: !Float
} deriving (Show)

instance Event EventTouchBegin where
  eventID _ = [C.pure| const StringHash* {&E_TOUCHBEGIN} |]
  loadEventData vmap = do
    tid <- variantMapGet' vmap [C.pure| const StringHash* {&TouchBegin::P_TOUCHID} |]
    tx <- variantMapGet' vmap [C.pure| const StringHash* {&TouchBegin::P_X} |]
    ty <- variantMapGet' vmap [C.pure| const StringHash* {&TouchBegin::P_Y} |]
    tp <- variantMapGet' vmap [C.pure| const StringHash* {&TouchBegin::P_PRESSURE} |]
    return $ EventTouchBegin {
      eventTouchId = fromMaybe 0 tid
    , eventTouchX = fromMaybe 0 tx
    , eventTouchY = fromMaybe 0 ty
    , eventTouchPressure = fromMaybe 0 tp
    }

-- | Finger released from the screen.
data EventTouchEnd = EventTouchEnd {
  eventTouchEndId  :: !Int
, eventTouchEndX   :: !Int
, eventTouchEndY   :: !Int
} deriving (Show)

instance Event EventTouchEnd where
  eventID _ = [C.pure| const StringHash* {&E_TOUCHEND} |]
  loadEventData vmap = do
    tid :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TouchEnd::P_TOUCHID} |]
    px :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TouchEnd::P_X} |]
    py :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TouchEnd::P_Y} |]
    return $ EventTouchEnd {
        eventTouchEndId = fromMaybe 0 tid
      , eventTouchEndX = fromMaybe 0 px
      , eventTouchEndY = fromMaybe 0 py
      }

-- | Finger moved on the screen.
data EventTouchMove = EventTouchMove {
  eventTouchMoveId        :: !Int
, eventTouchMoveX         :: !Int
, eventTouchMoveY         :: !Int
, eventTouchMoveDX        :: !Int
, eventTouchMoveDY        :: !Int
, eventTouchMovePressure  :: !Float
} deriving (Show)

instance Event EventTouchMove where
  eventID _ = [C.pure| const StringHash* {&E_TOUCHMOVE} |]
  loadEventData vmap = do
    tid :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TouchMove::P_TOUCHID} |]
    px :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TouchMove::P_X} |]
    py :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TouchMove::P_Y} |]
    pdx :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TouchMove::P_DX} |]
    pdy :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&TouchMove::P_DY} |]
    press :: Maybe Float <- variantMapGet' vmap [C.pure| const StringHash* {&TouchMove::P_PRESSURE} |]
    return $ EventTouchMove {
        eventTouchMoveId = fromMaybe 0 tid
      , eventTouchMoveX = fromMaybe 0 px
      , eventTouchMoveY = fromMaybe 0 py
      , eventTouchMoveDX = fromMaybe 0 pdx
      , eventTouchMoveDY = fromMaybe 0 pdy
      , eventTouchMovePressure = fromMaybe 0 press
      }

{-
TODO: Bind these

/// A touch gesture finished recording.
URHO3D_EVENT(E_GESTURERECORDED, GestureRecorded)
{
    URHO3D_PARAM(P_GESTUREID, GestureID);          // unsigned
}

/// A recognized touch gesture was input by the user.
URHO3D_EVENT(E_GESTUREINPUT, GestureInput)
{
    URHO3D_PARAM(P_GESTUREID, GestureID);          // unsigned
    URHO3D_PARAM(P_CENTERX, CenterX);              // int
    URHO3D_PARAM(P_CENTERY, CenterY);              // int
    URHO3D_PARAM(P_NUMFINGERS, NumFingers);        // int
    URHO3D_PARAM(P_ERROR, Error);                  // float
}

/// Pinch/rotate multi-finger touch gesture motion update.
URHO3D_EVENT(E_MULTIGESTURE, MultiGesture)
{
    URHO3D_PARAM(P_CENTERX, CenterX);              // int
    URHO3D_PARAM(P_CENTERY, CenterY);              // int
    URHO3D_PARAM(P_NUMFINGERS, NumFingers);        // int
    URHO3D_PARAM(P_DTHETA, DTheta);                // float (degrees)
    URHO3D_PARAM(P_DDIST, DDist);                  // float
}

/// A file was drag-dropped into the application window.
URHO3D_EVENT(E_DROPFILE, DropFile)
{
    URHO3D_PARAM(P_FILENAME, FileName);            // String
}

/// Application input focus or minimization changed.
URHO3D_EVENT(E_INPUTFOCUS, InputFocus)
{
    URHO3D_PARAM(P_FOCUS, Focus);                  // bool
    URHO3D_PARAM(P_MINIMIZED, Minimized);          // bool
}

/// OS mouse cursor visibility changed.
URHO3D_EVENT(E_MOUSEVISIBLECHANGED, MouseVisibleChanged)
{
    URHO3D_PARAM(P_VISIBLE, Visible);              // bool
}

/// Mouse mode changed.
URHO3D_EVENT(E_MOUSEMODECHANGED, MouseModeChanged)
{
    URHO3D_PARAM(P_MODE, Mode);                    // MouseMode
    URHO3D_PARAM(P_MOUSELOCKED, MouseLocked);      // bool
}

/// Application exit requested.
URHO3D_EVENT(E_EXITREQUESTED, ExitRequested)
{
}

/// Raw SDL input event.
URHO3D_EVENT(E_SDLRAWINPUT, SDLRawInput)
{
    URHO3D_PARAM(P_SDLEVENT, SDLEvent);           // SDL_Event*
    URHO3D_PARAM(P_CONSUMED, Consumed);           // bool
}

/// Input handling begins.
URHO3D_EVENT(E_INPUTBEGIN, InputBegin)
{
}

/// Input handling ends.
URHO3D_EVENT(E_INPUTEND, InputEnd)
{
}
-}
