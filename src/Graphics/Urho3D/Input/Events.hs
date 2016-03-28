module Graphics.Urho3D.Input.Events(
    EventKeyDown(..)
  , EventTouchBegin(..)
  , Key(..)
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Data.Monoid
import Data.Maybe 

C.context (C.cppCtx <> stringHashContext)
C.include "<Urho3D/Input/InputEvents.h>"
C.using "namespace Urho3D"

-- | Fires when user press down keyboard key
data EventKeyDown = EventKeyDown {
    pressKey :: Key 
  , pressScancode :: Int 
  , pressButtons :: Int 
  , pressQualifiers :: Int 
  , pressRepeat :: Bool
  } deriving (Show)

instance Event EventKeyDown where 
  eventID _ = [C.pure| const StringHash* {&E_KEYDOWN} |]
  loadEventData vmap = do 
    pkey <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_KEY} |]
    pscan <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_SCANCODE} |]
    pbuttons <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_BUTTONS} |]
    pqualifiers <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_QUALIFIERS} |]
    prepeat <- variantMapGet' vmap [C.pure| const StringHash* {&KeyDown::P_REPEAT} |]
    return $ EventKeyDown {
      pressKey = fromUrhoKey $ fromMaybe 0 pkey
    , pressScancode = fromMaybe 0 pscan 
    , pressButtons = fromMaybe 0 pbuttons
    , pressQualifiers = fromMaybe 0 pqualifiers
    , pressRepeat = fromMaybe False prepeat
    }

-- | Fires when user touches the screen
data EventTouchBegin = EventTouchBegin {
    eventTouchId :: Int 
  , eventTouchX :: Int 
  , eventTouchY :: Int 
  , eventTouchPressure :: Float
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

data Key = 
    KeyA 
  | KeyB 
  | KeyC
  | KeyD 
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ 
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeyBackspace
  | KeyTab
  | KeyReturn
  | KeyReturn2
  | KeyKPEnter 
  | KeyShift
  | KeyCtrl
  | KeyAlt
  | KeyGui
  | KeyPause
  | KeyCapsLock
  | KeyEsc
  | KeySpace
  | KeyPageUp
  | KeyPageDown
  | KeyEnd 
  | KeyHome
  | KeyLeft
  | KeyUp
  | KeyRight
  | KeyDown
  | KeySelect
  | KeyPrintScreen
  | KeyInsert
  | KeyDelete
  | KeyLGUI
  | KeyRGUI
  | KeyApplication
  | KeyKP0
  | KeyKP1
  | KeyKP2
  | KeyKP3
  | KeyKP4
  | KeyKP5
  | KeyKP6
  | KeyKP7
  | KeyKP8
  | KeyKP9
  | KeyKPMultiply
  | KeyKPPlus
  | KeyKPMinus
  | KeyKPPeriod
  | KeyKPDivide
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyNumLockClear
  | KeyScrollLock
  | KeyLShift
  | KeyRShift
  | KeyLCtrl
  | KeyRCtrl
  | KeyLAlt
  | KeyRAlt
  | KeyUnknown
  deriving (Eq, Ord, Show, Enum)

fromUrhoKey :: Int -> Key 
fromUrhoKey i 
  | i == keyA = KeyA 
  | i == keyB = KeyB 
  | i == keyC = KeyC
  | i == keyD = KeyD 
  | i == keyE = KeyE
  | i == keyF = KeyF
  | i == keyG = KeyG
  | i == keyH = KeyH
  | i == keyI = KeyI
  | i == keyJ = KeyJ
  | i == keyK = KeyK
  | i == keyL = KeyL
  | i == keyM = KeyM
  | i == keyN = KeyN
  | i == keyO = KeyO
  | i == keyP = KeyP
  | i == keyQ = KeyQ 
  | i == keyR = KeyR
  | i == keyS = KeyS
  | i == keyT = KeyT
  | i == keyU = KeyU
  | i == keyV = KeyV
  | i == keyW = KeyW
  | i == keyX = KeyX
  | i == keyY = KeyY
  | i == keyZ = KeyZ
  | i == key0 = Key0
  | i == key1 = Key1
  | i == key2 = Key2
  | i == key3 = Key3
  | i == key4 = Key4
  | i == key5 = Key5
  | i == key6 = Key6
  | i == key7 = Key7
  | i == key8 = Key8
  | i == key9 = Key9
  | i == keyBackspace = KeyBackspace
  | i == keyTab = KeyTab
  | i == keyReturn = KeyReturn
  | i == keyReturn2 = KeyReturn2
  | i == keyKPEnter = KeyKPEnter 
  | i == keyShift = KeyShift
  | i == keyCtrl = KeyCtrl
  | i == keyAlt = KeyAlt
  | i == keyGui = KeyGui
  | i == keyPause = KeyPause
  | i == keyCapsLock = KeyCapsLock
  | i == keyEsc = KeyEsc
  | i == keySpace = KeySpace
  | i == keyPageUp = KeyPageUp
  | i == keyPageDown = KeyPageDown
  | i == keyEnd = KeyEnd 
  | i == keyHome = KeyHome
  | i == keyLeft = KeyLeft
  | i == keyUp = KeyUp
  | i == keyRight = KeyRight
  | i == keyDown = KeyDown
  | i == keySelect = KeySelect
  | i == keyPrintScreen = KeyPrintScreen
  | i == keyInsert = KeyInsert
  | i == keyDelete = KeyDelete
  | i == keyLGUI = KeyLGUI
  | i == keyRGUI = KeyRGUI
  | i == keyApplication = KeyApplication
  | i == keyKP0 = KeyKP0
  | i == keyKP1 = KeyKP1
  | i == keyKP2 = KeyKP2
  | i == keyKP3 = KeyKP3
  | i == keyKP4 = KeyKP4
  | i == keyKP5 = KeyKP5
  | i == keyKP6 = KeyKP6
  | i == keyKP7 = KeyKP7
  | i == keyKP8 = KeyKP8
  | i == keyKP9 = KeyKP9
  | i == keyKPMultiply = KeyKPMultiply
  | i == keyKPPlus = KeyKPPlus
  | i == keyKPMinus = KeyKPMinus
  | i == keyKPPeriod = KeyKPPeriod
  | i == keyKPDivide = KeyKPDivide
  | i == keyF1 = KeyF1
  | i == keyF2 = KeyF2
  | i == keyF3 = KeyF3
  | i == keyF4 = KeyF4
  | i == keyF5 = KeyF5
  | i == keyF6 = KeyF6
  | i == keyF7 = KeyF7
  | i == keyF8 = KeyF8
  | i == keyF9 = KeyF9
  | i == keyF10 = KeyF10
  | i == keyF11 = KeyF11
  | i == keyF12 = KeyF12
  | i == keyF13 = KeyF13
  | i == keyF14 = KeyF14
  | i == keyF15 = KeyF15
  | i == keyF16 = KeyF16
  | i == keyF17 = KeyF17
  | i == keyF18 = KeyF18
  | i == keyF19 = KeyF19
  | i == keyF20 = KeyF20
  | i == keyF21 = KeyF21
  | i == keyF22 = KeyF22
  | i == keyF23 = KeyF23
  | i == keyF24 = KeyF24
  | i == keyNumLockClear = KeyNumLockClear
  | i == keyScrollLock = KeyScrollLock
  | i == keyLShift = KeyLShift
  | i == keyRShift = KeyRShift
  | i == keyLCtrl = KeyLCtrl
  | i == keyRCtrl = KeyRCtrl
  | i == keyLAlt = KeyLAlt
  | i == keyRAlt = KeyRAlt
  | otherwise = KeyUnknown
  where 
    keyA = fromIntegral $ [C.pure| int {KEY_A} |]
    keyB = fromIntegral $ [C.pure| int {KEY_B} |]
    keyC = fromIntegral $ [C.pure| int {KEY_C} |]
    keyD = fromIntegral $ [C.pure| int {KEY_D} |]
    keyE = fromIntegral $ [C.pure| int {KEY_E} |]
    keyF = fromIntegral $ [C.pure| int {KEY_F} |]
    keyG = fromIntegral $ [C.pure| int {KEY_G} |]
    keyH = fromIntegral $ [C.pure| int {KEY_H} |]
    keyI = fromIntegral $ [C.pure| int {KEY_I} |]
    keyJ = fromIntegral $ [C.pure| int {KEY_J} |]
    keyK = fromIntegral $ [C.pure| int {KEY_K} |]
    keyL = fromIntegral $ [C.pure| int {KEY_L} |]
    keyM = fromIntegral $ [C.pure| int {KEY_M} |]
    keyN = fromIntegral $ [C.pure| int {KEY_N} |]
    keyO = fromIntegral $ [C.pure| int {KEY_O} |]
    keyP = fromIntegral $ [C.pure| int {KEY_P} |]
    keyQ = fromIntegral $ [C.pure| int {KEY_Q} |]
    keyR = fromIntegral $ [C.pure| int {KEY_R} |]
    keyS = fromIntegral $ [C.pure| int {KEY_S} |]
    keyT = fromIntegral $ [C.pure| int {KEY_T} |]
    keyU = fromIntegral $ [C.pure| int {KEY_U} |]
    keyV = fromIntegral $ [C.pure| int {KEY_V} |]
    keyW = fromIntegral $ [C.pure| int {KEY_W} |]
    keyX = fromIntegral $ [C.pure| int {KEY_X} |]
    keyY = fromIntegral $ [C.pure| int {KEY_Y} |]
    keyZ = fromIntegral $ [C.pure| int {KEY_Z} |]
    key0 = fromIntegral $ [C.pure| int {KEY_0} |]
    key1 = fromIntegral $ [C.pure| int {KEY_1} |]
    key2 = fromIntegral $ [C.pure| int {KEY_2} |]
    key3 = fromIntegral $ [C.pure| int {KEY_3} |]
    key4 = fromIntegral $ [C.pure| int {KEY_4} |]
    key5 = fromIntegral $ [C.pure| int {KEY_5} |]
    key6 = fromIntegral $ [C.pure| int {KEY_6} |]
    key7 = fromIntegral $ [C.pure| int {KEY_7} |]
    key8 = fromIntegral $ [C.pure| int {KEY_8} |]
    key9 = fromIntegral $ [C.pure| int {KEY_9} |]
    keyBackspace = fromIntegral $ [C.pure| int {KEY_BACKSPACE} |]
    keyTab = fromIntegral $ [C.pure| int {KEY_TAB} |]
    keyReturn = fromIntegral $ [C.pure| int {KEY_RETURN} |]
    keyReturn2 = fromIntegral $ [C.pure| int {KEY_RETURN2} |]
    keyKPEnter = fromIntegral $ [C.pure| int {KEY_KP_ENTER} |]
    keyShift = fromIntegral $ [C.pure| int {KEY_SHIFT} |]
    keyCtrl = fromIntegral $ [C.pure| int {KEY_CTRL} |]
    keyAlt = fromIntegral $ [C.pure| int {KEY_ALT} |]
    keyGui = fromIntegral $ [C.pure| int {KEY_GUI} |]
    keyPause = fromIntegral $ [C.pure| int {KEY_PAUSE} |]
    keyCapsLock = fromIntegral $ [C.pure| int {KEY_CAPSLOCK} |]
    keyEsc = fromIntegral $ [C.pure| int {KEY_ESC} |]
    keySpace = fromIntegral $ [C.pure| int {KEY_SPACE} |]
    keyPageUp = fromIntegral $ [C.pure| int {KEY_PAGEUP} |]
    keyPageDown = fromIntegral $ [C.pure| int {KEY_PAGEDOWN} |]
    keyEnd = fromIntegral $ [C.pure| int {KEY_END} |]
    keyHome = fromIntegral $ [C.pure| int {KEY_HOME} |]
    keyLeft = fromIntegral $ [C.pure| int {KEY_LEFT} |]
    keyUp = fromIntegral $ [C.pure| int {KEY_UP} |]
    keyRight = fromIntegral $ [C.pure| int {KEY_RIGHT} |]
    keyDown = fromIntegral $ [C.pure| int {KEY_DOWN} |]
    keySelect = fromIntegral $ [C.pure| int {KEY_SELECT} |]
    keyPrintScreen = fromIntegral $ [C.pure| int {KEY_PRINTSCREEN} |]
    keyInsert = fromIntegral $ [C.pure| int {KEY_INSERT} |]
    keyDelete = fromIntegral $ [C.pure| int {KEY_DELETE} |]
    keyLGUI = fromIntegral $ [C.pure| int {KEY_LGUI} |]
    keyRGUI = fromIntegral $ [C.pure| int {KEY_RGUI} |]
    keyApplication = fromIntegral $ [C.pure| int {KEY_APPLICATION} |]
    keyKP0 = fromIntegral $ [C.pure| int {KEY_KP_0} |]
    keyKP1 = fromIntegral $ [C.pure| int {KEY_KP_1} |]
    keyKP2 = fromIntegral $ [C.pure| int {KEY_KP_2} |]
    keyKP3 = fromIntegral $ [C.pure| int {KEY_KP_3} |]
    keyKP4 = fromIntegral $ [C.pure| int {KEY_KP_4} |]
    keyKP5 = fromIntegral $ [C.pure| int {KEY_KP_5} |]
    keyKP6 = fromIntegral $ [C.pure| int {KEY_KP_6} |]
    keyKP7 = fromIntegral $ [C.pure| int {KEY_KP_7} |]
    keyKP8 = fromIntegral $ [C.pure| int {KEY_KP_8} |]
    keyKP9 = fromIntegral $ [C.pure| int {KEY_KP_9} |]
    keyKPMultiply = fromIntegral $ [C.pure| int {KEY_KP_MULTIPLY} |]
    keyKPPlus = fromIntegral $ [C.pure| int {KEY_KP_PLUS} |]
    keyKPMinus = fromIntegral $ [C.pure| int {KEY_KP_MINUS} |]
    keyKPPeriod = fromIntegral $ [C.pure| int {KEY_KP_PERIOD} |]
    keyKPDivide = fromIntegral $ [C.pure| int {KEY_KP_DIVIDE} |]
    keyF1 = fromIntegral $ [C.pure| int {KEY_F1} |]
    keyF2 = fromIntegral $ [C.pure| int {KEY_F2} |]
    keyF3 = fromIntegral $ [C.pure| int {KEY_F3} |]
    keyF4 = fromIntegral $ [C.pure| int {KEY_F4} |]
    keyF5 = fromIntegral $ [C.pure| int {KEY_F5} |]
    keyF6 = fromIntegral $ [C.pure| int {KEY_F6} |]
    keyF7 = fromIntegral $ [C.pure| int {KEY_F7} |]
    keyF8 = fromIntegral $ [C.pure| int {KEY_F8} |]
    keyF9 = fromIntegral $ [C.pure| int {KEY_F9} |]
    keyF10 = fromIntegral $ [C.pure| int {KEY_F10} |]
    keyF11 = fromIntegral $ [C.pure| int {KEY_F11} |]
    keyF12 = fromIntegral $ [C.pure| int {KEY_F12} |]
    keyF13 = fromIntegral $ [C.pure| int {KEY_F13} |]
    keyF14 = fromIntegral $ [C.pure| int {KEY_F14} |]
    keyF15 = fromIntegral $ [C.pure| int {KEY_F15} |]
    keyF16 = fromIntegral $ [C.pure| int {KEY_F16} |]
    keyF17 = fromIntegral $ [C.pure| int {KEY_F17} |]
    keyF18 = fromIntegral $ [C.pure| int {KEY_F18} |]
    keyF19 = fromIntegral $ [C.pure| int {KEY_F19} |]
    keyF20 = fromIntegral $ [C.pure| int {KEY_F20} |]
    keyF21 = fromIntegral $ [C.pure| int {KEY_F21} |]
    keyF22 = fromIntegral $ [C.pure| int {KEY_F22} |]
    keyF23 = fromIntegral $ [C.pure| int {KEY_F23} |]
    keyF24 = fromIntegral $ [C.pure| int {KEY_F24} |]
    keyNumLockClear = fromIntegral $ [C.pure| int {KEY_NUMLOCKCLEAR} |]
    keyScrollLock = fromIntegral $ [C.pure| int {KEY_SCROLLLOCK} |]
    keyLShift = fromIntegral $ [C.pure| int {KEY_LSHIFT} |]
    keyRShift = fromIntegral $ [C.pure| int {KEY_RSHIFT} |]
    keyLCtrl = fromIntegral $ [C.pure| int {KEY_LCTRL} |]
    keyRCtrl = fromIntegral $ [C.pure| int {KEY_RCTRL} |]
    keyLAlt = fromIntegral $ [C.pure| int {KEY_LALT} |]
    keyRAlt = fromIntegral $ [C.pure| int {KEY_RALT} |]