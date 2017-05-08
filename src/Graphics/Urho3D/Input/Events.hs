module Graphics.Urho3D.Input.Events(
    EventKeyDown(..)
  , EventKeyUp(..)
  , EventTouchBegin(..)
  , Key(..)
  , toUrhoKey
  , fromUrhoKey
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

-- | Fires when user release keyboard key
data EventKeyUp = EventKeyUp {
  upKey        :: Key
, upScancode   :: Int
, upButtons    :: Int
, upQualifiers :: Int
} deriving (Show)

instance Event EventKeyUp where
  eventID _ = [C.pure| const StringHash* {&E_KEYUP} |]
  loadEventData vmap = do
    pkey <- variantMapGet' vmap [C.pure| const StringHash* {&KeyUp::P_KEY} |]
    pscan <- variantMapGet' vmap [C.pure| const StringHash* {&KeyUp::P_SCANCODE} |]
    pbuttons <- variantMapGet' vmap [C.pure| const StringHash* {&KeyUp::P_BUTTONS} |]
    pqualifiers <- variantMapGet' vmap [C.pure| const StringHash* {&KeyUp::P_QUALIFIERS} |]
    return $ EventKeyUp {
      upKey = fromUrhoKey $ fromMaybe 0 pkey
    , upScancode = fromMaybe 0 pscan
    , upButtons = fromMaybe 0 pbuttons
    , upQualifiers = fromMaybe 0 pqualifiers
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

toUrhoKey :: Key -> Int
toUrhoKey i
  | i == KeyA = keyA
  | i == KeyB = keyB
  | i == KeyC = keyC
  | i == KeyD = keyD
  | i == KeyE = keyE
  | i == KeyF = keyF
  | i == KeyG = keyG
  | i == KeyH = keyH
  | i == KeyI = keyI
  | i == KeyJ = keyJ
  | i == KeyK = keyK
  | i == KeyL = keyL
  | i == KeyM = keyM
  | i == KeyN = keyN
  | i == KeyO = keyO
  | i == KeyP = keyP
  | i == KeyQ = keyQ
  | i == KeyR = keyR
  | i == KeyS = keyS
  | i == KeyT = keyT
  | i == KeyU = keyU
  | i == KeyV = keyV
  | i == KeyW = keyW
  | i == KeyX = keyX
  | i == KeyY = keyY
  | i == KeyZ = keyZ
  | i == Key0 = key0
  | i == Key1 = key1
  | i == Key2 = key2
  | i == Key3 = key3
  | i == Key4 = key4
  | i == Key5 = key5
  | i == Key6 = key6
  | i == Key7 = key7
  | i == Key8 = key8
  | i == Key9 = key9
  | i == KeyBackspace = keyBackspace
  | i == KeyTab = keyTab
  | i == KeyReturn = keyReturn
  | i == KeyReturn2 = keyReturn2
  | i == KeyKPEnter  = keyKPEnter
  | i == KeyShift = keyShift
  | i == KeyCtrl = keyCtrl
  | i == KeyAlt = keyAlt
  | i == KeyGui = keyGui
  | i == KeyPause = keyPause
  | i == KeyCapsLock = keyCapsLock
  | i == KeyEsc = keyEsc
  | i == KeySpace = keySpace
  | i == KeyPageUp = keyPageUp
  | i == KeyPageDown = keyPageDown
  | i == KeyEnd  = keyEnd
  | i == KeyHome = keyHome
  | i == KeyLeft = keyLeft
  | i == KeyUp = keyUp
  | i == KeyRight = keyRight
  | i == KeyDown = keyDown
  | i == KeySelect = keySelect
  | i == KeyPrintScreen = keyPrintScreen
  | i == KeyInsert = keyInsert
  | i == KeyDelete = keyDelete
  | i == KeyLGUI = keyLGUI
  | i == KeyRGUI = keyRGUI
  | i == KeyApplication = keyApplication
  | i == KeyKP0 = keyKP0
  | i == KeyKP1 = keyKP1
  | i == KeyKP2 = keyKP2
  | i == KeyKP3 = keyKP3
  | i == KeyKP4 = keyKP4
  | i == KeyKP5 = keyKP5
  | i == KeyKP6 = keyKP6
  | i == KeyKP7 = keyKP7
  | i == KeyKP8 = keyKP8
  | i == KeyKP9 = keyKP9
  | i == KeyKPMultiply = keyKPMultiply
  | i == KeyKPPlus = keyKPPlus
  | i == KeyKPMinus = keyKPMinus
  | i == KeyKPPeriod = keyKPPeriod
  | i == KeyKPDivide = keyKPDivide
  | i == KeyF1 = keyF1
  | i == KeyF2 = keyF2
  | i == KeyF3 = keyF3
  | i == KeyF4 = keyF4
  | i == KeyF5 = keyF5
  | i == KeyF6 = keyF6
  | i == KeyF7 = keyF7
  | i == KeyF8 = keyF8
  | i == KeyF9 = keyF9
  | i == KeyF10 = keyF10
  | i == KeyF11 = keyF11
  | i == KeyF12 = keyF12
  | i == KeyF13 = keyF13
  | i == KeyF14 = keyF14
  | i == KeyF15 = keyF15
  | i == KeyF16 = keyF16
  | i == KeyF17 = keyF17
  | i == KeyF18 = keyF18
  | i == KeyF19 = keyF19
  | i == KeyF20 = keyF20
  | i == KeyF21 = keyF21
  | i == KeyF22 = keyF22
  | i == KeyF23 = keyF23
  | i == KeyF24 = keyF24
  | i == KeyNumLockClear = keyNumLockClear
  | i == KeyScrollLock = keyScrollLock
  | i == KeyLShift = keyLShift
  | i == KeyRShift = keyRShift
  | i == KeyLCtrl = keyLCtrl
  | i == KeyRCtrl = keyRCtrl
  | i == KeyLAlt = keyLAlt
  | i == KeyRAlt = keyRAlt
  | otherwise = 0

keyA :: Int
keyA = fromIntegral $ [C.pure| int {KEY_A} |]

keyB :: Int
keyB = fromIntegral $ [C.pure| int {KEY_B} |]

keyC :: Int
keyC = fromIntegral $ [C.pure| int {KEY_C} |]

keyD :: Int
keyD = fromIntegral $ [C.pure| int {KEY_D} |]

keyE :: Int
keyE = fromIntegral $ [C.pure| int {KEY_E} |]

keyF :: Int
keyF = fromIntegral $ [C.pure| int {KEY_F} |]

keyG :: Int
keyG = fromIntegral $ [C.pure| int {KEY_G} |]

keyH :: Int
keyH = fromIntegral $ [C.pure| int {KEY_H} |]

keyI :: Int
keyI = fromIntegral $ [C.pure| int {KEY_I} |]

keyJ :: Int
keyJ = fromIntegral $ [C.pure| int {KEY_J} |]

keyK :: Int
keyK = fromIntegral $ [C.pure| int {KEY_K} |]

keyL :: Int
keyL = fromIntegral $ [C.pure| int {KEY_L} |]

keyM :: Int
keyM = fromIntegral $ [C.pure| int {KEY_M} |]

keyN :: Int
keyN = fromIntegral $ [C.pure| int {KEY_N} |]

keyO :: Int
keyO = fromIntegral $ [C.pure| int {KEY_O} |]

keyP :: Int
keyP = fromIntegral $ [C.pure| int {KEY_P} |]

keyQ :: Int
keyQ = fromIntegral $ [C.pure| int {KEY_Q} |]

keyR :: Int
keyR = fromIntegral $ [C.pure| int {KEY_R} |]

keyS :: Int
keyS = fromIntegral $ [C.pure| int {KEY_S} |]

keyT :: Int
keyT = fromIntegral $ [C.pure| int {KEY_T} |]

keyU :: Int
keyU = fromIntegral $ [C.pure| int {KEY_U} |]

keyV :: Int
keyV = fromIntegral $ [C.pure| int {KEY_V} |]

keyW :: Int
keyW = fromIntegral $ [C.pure| int {KEY_W} |]

keyX :: Int
keyX = fromIntegral $ [C.pure| int {KEY_X} |]

keyY :: Int
keyY = fromIntegral $ [C.pure| int {KEY_Y} |]

keyZ :: Int
keyZ = fromIntegral $ [C.pure| int {KEY_Z} |]

key0 :: Int
key0 = fromIntegral $ [C.pure| int {KEY_0} |]

key1 :: Int
key1 = fromIntegral $ [C.pure| int {KEY_1} |]

key2 :: Int
key2 = fromIntegral $ [C.pure| int {KEY_2} |]

key3 :: Int
key3 = fromIntegral $ [C.pure| int {KEY_3} |]

key4 :: Int
key4 = fromIntegral $ [C.pure| int {KEY_4} |]

key5 :: Int
key5 = fromIntegral $ [C.pure| int {KEY_5} |]

key6 :: Int
key6 = fromIntegral $ [C.pure| int {KEY_6} |]

key7 :: Int
key7 = fromIntegral $ [C.pure| int {KEY_7} |]

key8 :: Int
key8 = fromIntegral $ [C.pure| int {KEY_8} |]

key9 :: Int
key9 = fromIntegral $ [C.pure| int {KEY_9} |]

keyBackspace :: Int
keyBackspace = fromIntegral $ [C.pure| int {KEY_BACKSPACE} |]

keyTab :: Int
keyTab = fromIntegral $ [C.pure| int {KEY_TAB} |]

keyReturn :: Int
keyReturn = fromIntegral $ [C.pure| int {KEY_RETURN} |]

keyReturn2 :: Int
keyReturn2 = fromIntegral $ [C.pure| int {KEY_RETURN2} |]

keyKPEnter :: Int
keyKPEnter = fromIntegral $ [C.pure| int {KEY_KP_ENTER} |]

keyShift :: Int
keyShift = fromIntegral $ [C.pure| int {KEY_SHIFT} |]

keyCtrl :: Int
keyCtrl = fromIntegral $ [C.pure| int {KEY_CTRL} |]

keyAlt :: Int
keyAlt = fromIntegral $ [C.pure| int {KEY_ALT} |]

keyGui :: Int
keyGui = fromIntegral $ [C.pure| int {KEY_GUI} |]

keyPause :: Int
keyPause = fromIntegral $ [C.pure| int {KEY_PAUSE} |]

keyCapsLock :: Int
keyCapsLock = fromIntegral $ [C.pure| int {KEY_CAPSLOCK} |]

keyEsc :: Int
keyEsc = fromIntegral $ [C.pure| int {KEY_ESCAPE} |]

keySpace :: Int
keySpace = fromIntegral $ [C.pure| int {KEY_SPACE} |]

keyPageUp :: Int
keyPageUp = fromIntegral $ [C.pure| int {KEY_PAGEUP} |]

keyPageDown :: Int
keyPageDown = fromIntegral $ [C.pure| int {KEY_PAGEDOWN} |]

keyEnd :: Int
keyEnd = fromIntegral $ [C.pure| int {KEY_END} |]

keyHome :: Int
keyHome = fromIntegral $ [C.pure| int {KEY_HOME} |]

keyLeft :: Int
keyLeft = fromIntegral $ [C.pure| int {KEY_LEFT} |]

keyUp :: Int
keyUp = fromIntegral $ [C.pure| int {KEY_UP} |]

keyRight :: Int
keyRight = fromIntegral $ [C.pure| int {KEY_RIGHT} |]

keyDown :: Int
keyDown = fromIntegral $ [C.pure| int {KEY_DOWN} |]

keySelect :: Int
keySelect = fromIntegral $ [C.pure| int {KEY_SELECT} |]

keyPrintScreen :: Int
keyPrintScreen = fromIntegral $ [C.pure| int {KEY_PRINTSCREEN} |]

keyInsert :: Int
keyInsert = fromIntegral $ [C.pure| int {KEY_INSERT} |]

keyDelete :: Int
keyDelete = fromIntegral $ [C.pure| int {KEY_DELETE} |]

keyLGUI :: Int
keyLGUI = fromIntegral $ [C.pure| int {KEY_LGUI} |]

keyRGUI :: Int
keyRGUI = fromIntegral $ [C.pure| int {KEY_RGUI} |]

keyApplication :: Int
keyApplication = fromIntegral $ [C.pure| int {KEY_APPLICATION} |]

keyKP0 :: Int
keyKP0 = fromIntegral $ [C.pure| int {KEY_KP_0} |]

keyKP1 :: Int
keyKP1 = fromIntegral $ [C.pure| int {KEY_KP_1} |]

keyKP2 :: Int
keyKP2 = fromIntegral $ [C.pure| int {KEY_KP_2} |]

keyKP3 :: Int
keyKP3 = fromIntegral $ [C.pure| int {KEY_KP_3} |]

keyKP4 :: Int
keyKP4 = fromIntegral $ [C.pure| int {KEY_KP_4} |]

keyKP5 :: Int
keyKP5 = fromIntegral $ [C.pure| int {KEY_KP_5} |]

keyKP6 :: Int
keyKP6 = fromIntegral $ [C.pure| int {KEY_KP_6} |]

keyKP7 :: Int
keyKP7 = fromIntegral $ [C.pure| int {KEY_KP_7} |]

keyKP8 :: Int
keyKP8 = fromIntegral $ [C.pure| int {KEY_KP_8} |]

keyKP9 :: Int
keyKP9 = fromIntegral $ [C.pure| int {KEY_KP_9} |]

keyKPMultiply :: Int
keyKPMultiply = fromIntegral $ [C.pure| int {KEY_KP_MULTIPLY} |]

keyKPPlus :: Int
keyKPPlus = fromIntegral $ [C.pure| int {KEY_KP_PLUS} |]

keyKPMinus :: Int
keyKPMinus = fromIntegral $ [C.pure| int {KEY_KP_MINUS} |]

keyKPPeriod :: Int
keyKPPeriod = fromIntegral $ [C.pure| int {KEY_KP_PERIOD} |]

keyKPDivide :: Int
keyKPDivide = fromIntegral $ [C.pure| int {KEY_KP_DIVIDE} |]

keyF1 :: Int
keyF1 = fromIntegral $ [C.pure| int {KEY_F1} |]

keyF2 :: Int
keyF2 = fromIntegral $ [C.pure| int {KEY_F2} |]

keyF3 :: Int
keyF3 = fromIntegral $ [C.pure| int {KEY_F3} |]

keyF4 :: Int
keyF4 = fromIntegral $ [C.pure| int {KEY_F4} |]

keyF5 :: Int
keyF5 = fromIntegral $ [C.pure| int {KEY_F5} |]

keyF6 :: Int
keyF6 = fromIntegral $ [C.pure| int {KEY_F6} |]

keyF7 :: Int
keyF7 = fromIntegral $ [C.pure| int {KEY_F7} |]

keyF8 :: Int
keyF8 = fromIntegral $ [C.pure| int {KEY_F8} |]

keyF9 :: Int
keyF9 = fromIntegral $ [C.pure| int {KEY_F9} |]

keyF10 :: Int
keyF10 = fromIntegral $ [C.pure| int {KEY_F10} |]

keyF11 :: Int
keyF11 = fromIntegral $ [C.pure| int {KEY_F11} |]

keyF12 :: Int
keyF12 = fromIntegral $ [C.pure| int {KEY_F12} |]

keyF13 :: Int
keyF13 = fromIntegral $ [C.pure| int {KEY_F13} |]

keyF14 :: Int
keyF14 = fromIntegral $ [C.pure| int {KEY_F14} |]

keyF15 :: Int
keyF15 = fromIntegral $ [C.pure| int {KEY_F15} |]

keyF16 :: Int
keyF16 = fromIntegral $ [C.pure| int {KEY_F16} |]

keyF17 :: Int
keyF17 = fromIntegral $ [C.pure| int {KEY_F17} |]

keyF18 :: Int
keyF18 = fromIntegral $ [C.pure| int {KEY_F18} |]

keyF19 :: Int
keyF19 = fromIntegral $ [C.pure| int {KEY_F19} |]

keyF20 :: Int
keyF20 = fromIntegral $ [C.pure| int {KEY_F20} |]

keyF21 :: Int
keyF21 = fromIntegral $ [C.pure| int {KEY_F21} |]

keyF22 :: Int
keyF22 = fromIntegral $ [C.pure| int {KEY_F22} |]

keyF23 :: Int
keyF23 = fromIntegral $ [C.pure| int {KEY_F23} |]

keyF24 :: Int
keyF24 = fromIntegral $ [C.pure| int {KEY_F24} |]

keyNumLockClear :: Int
keyNumLockClear = fromIntegral $ [C.pure| int {KEY_NUMLOCKCLEAR} |]

keyScrollLock :: Int
keyScrollLock = fromIntegral $ [C.pure| int {KEY_SCROLLLOCK} |]

keyLShift :: Int
keyLShift = fromIntegral $ [C.pure| int {KEY_LSHIFT} |]

keyRShift :: Int
keyRShift = fromIntegral $ [C.pure| int {KEY_RSHIFT} |]

keyLCtrl :: Int
keyLCtrl = fromIntegral $ [C.pure| int {KEY_LCTRL} |]

keyRCtrl :: Int
keyRCtrl = fromIntegral $ [C.pure| int {KEY_RCTRL} |]

keyLAlt :: Int
keyLAlt = fromIntegral $ [C.pure| int {KEY_LALT} |]

keyRAlt :: Int
keyRAlt = fromIntegral $ [C.pure| int {KEY_RALT} |]
