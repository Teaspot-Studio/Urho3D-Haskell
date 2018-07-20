module Graphics.Urho3D.Input.InputConstants(
    MouseButton(..)
  , MouseButtonFlags
  , Qualifier(..)
  , QualifierFlags
  , Key(..)
  , Scancode(..)
  , HatPosition(..)
  , ControllerButton(..)
  , ControllerAxis(..)
  ) where

import GHC.Generics
import Graphics.Urho3D.Container.FlagSet
import Data.Word

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

C.context (C.cppCtx)
C.include "<Urho3D/Input/InputConstants.h>"
C.using "namespace Urho3D"

data MouseButton =
    MouseButtonNone
  | MouseButtonLeft
  | MouseButtonMiddle
  | MouseButtonRight
  | MouseButtonX1
  | MouseButtonX2
  | MouseButtonAny
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum MouseButton where
  toEnum = fromUrhoMouseButton
  {-# INLINE toEnum #-}
  fromEnum = toUrhoMouseButton
  {-# INLINE fromEnum #-}

fromUrhoMouseButton :: Int -> MouseButton
fromUrhoMouseButton i
  | i == mouseButtonNone = MouseButtonNone
  | i == mouseButtonLeft = MouseButtonLeft
  | i == mouseButtonMiddle = MouseButtonMiddle
  | i == mouseButtonRight = MouseButtonRight
  | i == mouseButtonX1 = MouseButtonX1
  | i == mouseButtonX2 = MouseButtonX2
  | i == mouseButtonAny = MouseButtonAny
  | otherwise = MouseButtonNone

toUrhoMouseButton :: MouseButton -> Int
toUrhoMouseButton i = case i of
  MouseButtonNone -> mouseButtonNone
  MouseButtonLeft -> mouseButtonLeft
  MouseButtonMiddle -> mouseButtonMiddle
  MouseButtonRight -> mouseButtonRight
  MouseButtonX1 -> mouseButtonX1
  MouseButtonX2 -> mouseButtonX2
  MouseButtonAny -> mouseButtonAny

type MouseButtonFlags = FlagSet Word32 MouseButton

mouseButtonNone :: Int
mouseButtonNone = fromIntegral $ [C.pure| int { MOUSEB_NONE } |]

mouseButtonLeft :: Int
mouseButtonLeft = fromIntegral $ [C.pure| int { MOUSEB_LEFT } |]

mouseButtonMiddle :: Int
mouseButtonMiddle = fromIntegral $ [C.pure| int { MOUSEB_MIDDLE } |]

mouseButtonRight :: Int
mouseButtonRight = fromIntegral $ [C.pure| int { MOUSEB_RIGHT } |]

mouseButtonX1 :: Int
mouseButtonX1 = fromIntegral $ [C.pure| int { MOUSEB_X1 } |]

mouseButtonX2 :: Int
mouseButtonX2 = fromIntegral $ [C.pure| int { MOUSEB_X2 } |]

mouseButtonAny :: Int
mouseButtonAny = fromIntegral $ [C.pure| int { MOUSEB_ANY } |]

data Qualifier =
    QualNone
  | QualShift
  | QualCtrl
  | QualAlt
  | QualAny
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum Qualifier where
  toEnum i = case i of
    0 -> QualNone
    1 -> QualShift
    2 -> QualCtrl
    4 -> QualAlt
    8 -> QualAny
    _ -> QualNone
  {-# INLINE toEnum #-}
  fromEnum i = case i of
    QualNone -> 0
    QualShift -> 1
    QualCtrl -> 2
    QualAlt -> 4
    QualAny -> 8
  {-# INLINE fromEnum #-}

type QualifierFlags = FlagSet Word32 Qualifier

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
  | KeyACBack
  | KeyACBookMarks
  | KeyACForward
  | KeyACHome
  | KeyACRefresh
  | KeyACSearch
  | KeyACStop
  | KeyAgain
  | KeyAltErase
  | KeyAmpersand
  | KeyAT
  | KeyAudioMute
  | KeyAudioNext
  | KeyAudioPlay
  | KeyAudioPrev
  | KeyAudioStop
  | KeyBackQuote
  | KeyBackSlash
  | KeyBrightnessDown
  | KeyBrightnessUp
  | KeyCalculator
  | KeyCancel
  | KeyCaret
  | KeyClear
  | KeyClearAgain
  | KeyColon
  | KeyComma
  | KeyComputer
  | KeyCopy
  | KeyCRSel
  | KeyCurrencySubUnit
  | KeyCurrencyUnit
  | KeyCut
  | KeyDecimalSeparator
  | KeyDisplaySwitch
  | KeyDollar
  | KeyEject
  | KeyEquals
  | KeyExclaim
  | KeyExsel
  | KeyFind
  | KeyGreater
  | KeyHash
  | KeyHelp
  | KeyKBDillumDown
  | KeyKBDillumToggle
  | KeyKBDillumUp
  | KeyKP00
  | KeyKP000
  | KeyKPA
  | KeyKPAmpersand
  | KeyKPAT
  | KeyKPB
  | KeyKPBackspace
  | KeyKPBinary
  | KeyKPC
  | KeyKPClear
  | KeyKPClearEntry
  | KeyKPColon
  | KeyKPComma
  | KeyKPD
  | KeyKPDBLAmpersand
  | KeyKPDBLVerticalBar
  | KeyKPDecimal
  | KeyKPE
  | KeyKPEquals
  | KeyKPEqualsAS400
  | KeyKPExclaim
  | KeyKPF
  | KeyKPGreater
  | KeyKPHash
  | KeyKPHexadecimal
  | KeyKPLeftBrace
  | KeyKPLeftParen
  | KeyKPLess
  | KeyKPMemAdd
  | KeyKPMemClear
  | KeyKPMemDivide
  | KeyKPMemMultiply
  | KeyKPMemRecall
  | KeyKPMemStore
  | KeyKPMemSubtract
  | KeyKPOctal
  | KeyKPPercent
  | KeyKPPlusMinus
  | KeyKPPower
  | KeyKPRightBrace
  | KeyKPRightParen
  | KeyKPSpace
  | KeyKPTab
  | KeyKPVerticalBar
  | KeyKPXor
  | KeyLeftBracket
  | KeyLeftParen
  | KeyLess
  | KeyMail
  | KeyMediaSelect
  | KeyMenu
  | KeyMinus
  | KeyMode
  | KeyMute
  | KeyOper
  | KeyOut
  | KeyPaste
  | KeyPercent
  | KeyPeriod
  | KeyPlus
  | KeyPower
  | KeyPrior
  | KeyQuestion
  | KeyQuote
  | KeyQuoteDbl
  | KeyRightBracket
  | KeyRightParen
  | KeySemicolon
  | KeySeparator
  | KeySlash
  | KeySleep
  | KeyStop
  | KeySysReq
  | KeyThousandsSeparator
  | KeyUnderscore
  | KeyUndo
  | KeyVolumeDown
  | KeyVolumeUp
  | KeyWWW
  | KeyUnknown
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum Key where
  fromEnum = toUrhoKey
  {-# INLINE fromEnum #-}
  toEnum = fromUrhoKey
  {-# INLINE toEnum #-}

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
  | i == keyACBack = KeyACBack
  | i == keyACBookMarks = KeyACBookMarks
  | i == keyACForward = KeyACForward
  | i == keyACHome = KeyACHome
  | i == keyACRefresh = KeyACRefresh
  | i == keyACSearch = KeyACSearch
  | i == keyACStop = KeyACStop
  | i == keyAgain = KeyAgain
  | i == keyAltErase = KeyAltErase
  | i == keyAmpersand = KeyAmpersand
  | i == keyAT = KeyAT
  | i == keyAudioMute = KeyAudioMute
  | i == keyAudioNext = KeyAudioNext
  | i == keyAudioPlay = KeyAudioPlay
  | i == keyAudioPrev = KeyAudioPrev
  | i == keyAudioStop = KeyAudioStop
  | i == keyBackQuote = KeyBackQuote
  | i == keyBackSlash = KeyBackSlash
  | i == keyBrightnessDown = KeyBrightnessDown
  | i == keyBrightnessUp = KeyBrightnessUp
  | i == keyCalculator = KeyCalculator
  | i == keyCancel = KeyCancel
  | i == keyCaret = KeyCaret
  | i == keyClear = KeyClear
  | i == keyClearAgain = KeyClearAgain
  | i == keyColon = KeyColon
  | i == keyComma = KeyComma
  | i == keyComputer = KeyComputer
  | i == keyCopy = KeyCopy
  | i == keyCRSel = KeyCRSel
  | i == keyCurrencySubUnit = KeyCurrencySubUnit
  | i == keyCurrencyUnit = KeyCurrencyUnit
  | i == keyCut = KeyCut
  | i == keyDecimalSeparator = KeyDecimalSeparator
  | i == keyDisplaySwitch = KeyDisplaySwitch
  | i == keyDollar = KeyDollar
  | i == keyEject = KeyEject
  | i == keyEquals = KeyEquals
  | i == keyExclaim = KeyExclaim
  | i == keyExsel = KeyExsel
  | i == keyFind = KeyFind
  | i == keyGreater = KeyGreater
  | i == keyHash = KeyHash
  | i == keyHelp = KeyHelp
  | i == keyKBDillumDown = KeyKBDillumDown
  | i == keyKBDillumToggle = KeyKBDillumToggle
  | i == keyKBDillumUp = KeyKBDillumUp
  | i == keyKP00 = KeyKP00
  | i == keyKP000 = KeyKP000
  | i == keyKPA = KeyKPA
  | i == keyKPAmpersand = KeyKPAmpersand
  | i == keyKPAT = KeyKPAT
  | i == keyKPB = KeyKPB
  | i == keyKPBackspace = KeyKPBackspace
  | i == keyKPBinary = KeyKPBinary
  | i == keyKPC = KeyKPC
  | i == keyKPClear = KeyKPClear
  | i == keyKPClearEntry = KeyKPClearEntry
  | i == keyKPColon = KeyKPColon
  | i == keyKPComma = KeyKPComma
  | i == keyKPD = KeyKPD
  | i == keyKPDBLAmpersand = KeyKPDBLAmpersand
  | i == keyKPDBLVerticalBar = KeyKPDBLVerticalBar
  | i == keyKPDecimal = KeyKPDecimal
  | i == keyKPE = KeyKPE
  | i == keyKPEquals = KeyKPEquals
  | i == keyKPEqualsAS400 = KeyKPEqualsAS400
  | i == keyKPExclaim = KeyKPExclaim
  | i == keyKPF = KeyKPF
  | i == keyKPGreater = KeyKPGreater
  | i == keyKPHash = KeyKPHash
  | i == keyKPHexadecimal = KeyKPHexadecimal
  | i == keyKPLeftBrace = KeyKPLeftBrace
  | i == keyKPLeftParen = KeyKPLeftParen
  | i == keyKPLess = KeyKPLess
  | i == keyKPMemAdd = KeyKPMemAdd
  | i == keyKPMemClear = KeyKPMemClear
  | i == keyKPMemDivide = KeyKPMemDivide
  | i == keyKPMemMultiply = KeyKPMemMultiply
  | i == keyKPMemRecall = KeyKPMemRecall
  | i == keyKPMemStore = KeyKPMemStore
  | i == keyKPMemSubtract = KeyKPMemSubtract
  | i == keyKPOctal = KeyKPOctal
  | i == keyKPPercent = KeyKPPercent
  | i == keyKPPlusMinus = KeyKPPlusMinus
  | i == keyKPPower = KeyKPPower
  | i == keyKPRightBrace = KeyKPRightBrace
  | i == keyKPRightParen = KeyKPRightParen
  | i == keyKPSpace = KeyKPSpace
  | i == keyKPTab = KeyKPTab
  | i == keyKPVerticalBar = KeyKPVerticalBar
  | i == keyKPXor = KeyKPXor
  | i == keyLeftBracket = KeyLeftBracket
  | i == keyLeftParen = KeyLeftParen
  | i == keyLess = KeyLess
  | i == keyMail = KeyMail
  | i == keyMediaSelect = KeyMediaSelect
  | i == keyMenu = KeyMenu
  | i == keyMinus = KeyMinus
  | i == keyMode = KeyMode
  | i == keyMute = KeyMute
  | i == keyOper = KeyOper
  | i == keyOut = KeyOut
  | i == keyPaste = KeyPaste
  | i == keyPercent = KeyPercent
  | i == keyPeriod = KeyPeriod
  | i == keyPlus = KeyPlus
  | i == keyPower = KeyPower
  | i == keyPrior = KeyPrior
  | i == keyQuestion = KeyQuestion
  | i == keyQuote = KeyQuote
  | i == keyQuoteDbl = KeyQuoteDbl
  | i == keyRightBracket = KeyRightBracket
  | i == keyRightParen = KeyRightParen
  | i == keySemicolon = KeySemicolon
  | i == keySeparator = KeySeparator
  | i == keySlash = KeySlash
  | i == keySleep = KeySleep
  | i == keyStop = KeyStop
  | i == keySysReq = KeySysReq
  | i == keyThousandsSeparator = KeyThousandsSeparator
  | i == keyUnderscore = KeyUnderscore
  | i == keyUndo = KeyUndo
  | i == keyVolumeDown = KeyVolumeDown
  | i == keyVolumeUp = KeyVolumeUp
  | i == keyWWW = KeyWWW
  | otherwise = KeyUnknown

toUrhoKey :: Key -> Int
toUrhoKey i = case i of
  KeyA -> keyA
  KeyB -> keyB
  KeyC -> keyC
  KeyD -> keyD
  KeyE -> keyE
  KeyF -> keyF
  KeyG -> keyG
  KeyH -> keyH
  KeyI -> keyI
  KeyJ -> keyJ
  KeyK -> keyK
  KeyL -> keyL
  KeyM -> keyM
  KeyN -> keyN
  KeyO -> keyO
  KeyP -> keyP
  KeyQ -> keyQ
  KeyR -> keyR
  KeyS -> keyS
  KeyT -> keyT
  KeyU -> keyU
  KeyV -> keyV
  KeyW -> keyW
  KeyX -> keyX
  KeyY -> keyY
  KeyZ -> keyZ
  Key0 -> key0
  Key1 -> key1
  Key2 -> key2
  Key3 -> key3
  Key4 -> key4
  Key5 -> key5
  Key6 -> key6
  Key7 -> key7
  Key8 -> key8
  Key9 -> key9
  KeyBackspace -> keyBackspace
  KeyTab -> keyTab
  KeyReturn -> keyReturn
  KeyReturn2 -> keyReturn2
  KeyKPEnter -> keyKPEnter
  KeyShift -> keyShift
  KeyCtrl -> keyCtrl
  KeyAlt -> keyAlt
  KeyGui -> keyGui
  KeyPause -> keyPause
  KeyCapsLock -> keyCapsLock
  KeyEsc -> keyEsc
  KeySpace -> keySpace
  KeyPageUp -> keyPageUp
  KeyPageDown -> keyPageDown
  KeyEnd -> keyEnd
  KeyHome -> keyHome
  KeyLeft -> keyLeft
  KeyUp -> keyUp
  KeyRight -> keyRight
  KeyDown -> keyDown
  KeySelect -> keySelect
  KeyPrintScreen -> keyPrintScreen
  KeyInsert -> keyInsert
  KeyDelete -> keyDelete
  KeyLGUI -> keyLGUI
  KeyRGUI -> keyRGUI
  KeyApplication -> keyApplication
  KeyKP0 -> keyKP0
  KeyKP1 -> keyKP1
  KeyKP2 -> keyKP2
  KeyKP3 -> keyKP3
  KeyKP4 -> keyKP4
  KeyKP5 -> keyKP5
  KeyKP6 -> keyKP6
  KeyKP7 -> keyKP7
  KeyKP8 -> keyKP8
  KeyKP9 -> keyKP9
  KeyKPMultiply -> keyKPMultiply
  KeyKPPlus -> keyKPPlus
  KeyKPMinus -> keyKPMinus
  KeyKPPeriod -> keyKPPeriod
  KeyKPDivide -> keyKPDivide
  KeyF1 -> keyF1
  KeyF2 -> keyF2
  KeyF3 -> keyF3
  KeyF4 -> keyF4
  KeyF5 -> keyF5
  KeyF6 -> keyF6
  KeyF7 -> keyF7
  KeyF8 -> keyF8
  KeyF9 -> keyF9
  KeyF10 -> keyF10
  KeyF11 -> keyF11
  KeyF12 -> keyF12
  KeyF13 -> keyF13
  KeyF14 -> keyF14
  KeyF15 -> keyF15
  KeyF16 -> keyF16
  KeyF17 -> keyF17
  KeyF18 -> keyF18
  KeyF19 -> keyF19
  KeyF20 -> keyF20
  KeyF21 -> keyF21
  KeyF22 -> keyF22
  KeyF23 -> keyF23
  KeyF24 -> keyF24
  KeyNumLockClear -> keyNumLockClear
  KeyScrollLock -> keyScrollLock
  KeyLShift -> keyLShift
  KeyRShift -> keyRShift
  KeyLCtrl -> keyLCtrl
  KeyRCtrl -> keyRCtrl
  KeyLAlt -> keyLAlt
  KeyRAlt -> keyRAlt
  KeyACBack -> keyACBack
  KeyACBookMarks -> keyACBookMarks
  KeyACForward -> keyACForward
  KeyACHome -> keyACHome
  KeyACRefresh -> keyACRefresh
  KeyACSearch -> keyACSearch
  KeyACStop -> keyACStop
  KeyAgain -> keyAgain
  KeyAltErase -> keyAltErase
  KeyAmpersand -> keyAmpersand
  KeyAT -> keyAT
  KeyAudioMute -> keyAudioMute
  KeyAudioNext -> keyAudioNext
  KeyAudioPlay -> keyAudioPlay
  KeyAudioPrev -> keyAudioPrev
  KeyAudioStop -> keyAudioStop
  KeyBackQuote -> keyBackQuote
  KeyBackSlash -> keyBackSlash
  KeyBrightnessDown -> keyBrightnessDown
  KeyBrightnessUp -> keyBrightnessUp
  KeyCalculator -> keyCalculator
  KeyCancel -> keyCancel
  KeyCaret -> keyCaret
  KeyClear -> keyClear
  KeyClearAgain -> keyClearAgain
  KeyColon -> keyColon
  KeyComma -> keyComma
  KeyComputer -> keyComputer
  KeyCopy -> keyCopy
  KeyCRSel -> keyCRSel
  KeyCurrencySubUnit -> keyCurrencySubUnit
  KeyCurrencyUnit -> keyCurrencyUnit
  KeyCut -> keyCut
  KeyDecimalSeparator -> keyDecimalSeparator
  KeyDisplaySwitch -> keyDisplaySwitch
  KeyDollar -> keyDollar
  KeyEject -> keyEject
  KeyEquals -> keyEquals
  KeyExclaim -> keyExclaim
  KeyExsel -> keyExsel
  KeyFind -> keyFind
  KeyGreater -> keyGreater
  KeyHash -> keyHash
  KeyHelp -> keyHelp
  KeyKBDillumDown -> keyKBDillumDown
  KeyKBDillumToggle -> keyKBDillumToggle
  KeyKBDillumUp -> keyKBDillumUp
  KeyKP00 -> keyKP00
  KeyKP000 -> keyKP000
  KeyKPA -> keyKPA
  KeyKPAmpersand -> keyKPAmpersand
  KeyKPAT -> keyKPAT
  KeyKPB -> keyKPB
  KeyKPBackspace -> keyKPBackspace
  KeyKPBinary -> keyKPBinary
  KeyKPC -> keyKPC
  KeyKPClear -> keyKPClear
  KeyKPClearEntry -> keyKPClearEntry
  KeyKPColon -> keyKPColon
  KeyKPComma -> keyKPComma
  KeyKPD -> keyKPD
  KeyKPDBLAmpersand -> keyKPDBLAmpersand
  KeyKPDBLVerticalBar -> keyKPDBLVerticalBar
  KeyKPDecimal -> keyKPDecimal
  KeyKPE -> keyKPE
  KeyKPEquals -> keyKPEquals
  KeyKPEqualsAS400 -> keyKPEqualsAS400
  KeyKPExclaim -> keyKPExclaim
  KeyKPF -> keyKPF
  KeyKPGreater -> keyKPGreater
  KeyKPHash -> keyKPHash
  KeyKPHexadecimal -> keyKPHexadecimal
  KeyKPLeftBrace -> keyKPLeftBrace
  KeyKPLeftParen -> keyKPLeftParen
  KeyKPLess -> keyKPLess
  KeyKPMemAdd -> keyKPMemAdd
  KeyKPMemClear -> keyKPMemClear
  KeyKPMemDivide -> keyKPMemDivide
  KeyKPMemMultiply -> keyKPMemMultiply
  KeyKPMemRecall -> keyKPMemRecall
  KeyKPMemStore -> keyKPMemStore
  KeyKPMemSubtract -> keyKPMemSubtract
  KeyKPOctal -> keyKPOctal
  KeyKPPercent -> keyKPPercent
  KeyKPPlusMinus -> keyKPPlusMinus
  KeyKPPower -> keyKPPower
  KeyKPRightBrace -> keyKPRightBrace
  KeyKPRightParen -> keyKPRightParen
  KeyKPSpace -> keyKPSpace
  KeyKPTab -> keyKPTab
  KeyKPVerticalBar -> keyKPVerticalBar
  KeyKPXor -> keyKPXor
  KeyLeftBracket -> keyLeftBracket
  KeyLeftParen -> keyLeftParen
  KeyLess -> keyLess
  KeyMail -> keyMail
  KeyMediaSelect -> keyMediaSelect
  KeyMenu -> keyMenu
  KeyMinus -> keyMinus
  KeyMode -> keyMode
  KeyMute -> keyMute
  KeyOper -> keyOper
  KeyOut -> keyOut
  KeyPaste -> keyPaste
  KeyPercent -> keyPercent
  KeyPeriod -> keyPeriod
  KeyPlus -> keyPlus
  KeyPower -> keyPower
  KeyPrior -> keyPrior
  KeyQuestion -> keyQuestion
  KeyQuote -> keyQuote
  KeyQuoteDbl -> keyQuoteDbl
  KeyRightBracket -> keyRightBracket
  KeyRightParen -> keyRightParen
  KeySemicolon -> keySemicolon
  KeySeparator -> keySeparator
  KeySlash -> keySlash
  KeySleep -> keySleep
  KeyStop -> keyStop
  KeySysReq -> keySysReq
  KeyThousandsSeparator -> keyThousandsSeparator
  KeyUnderscore -> keyUnderscore
  KeyUndo -> keyUndo
  KeyVolumeDown -> keyVolumeDown
  KeyVolumeUp -> keyVolumeUp
  KeyWWW -> keyWWW
  KeyUnknown -> keyUnknown

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

keyACBack :: Int
keyACBack = fromIntegral $ [C.pure| int {KEY_AC_BACK} |]

keyACBookMarks :: Int
keyACBookMarks = fromIntegral $ [C.pure| int {KEY_AC_BOOKMARKS} |]

keyACForward :: Int
keyACForward = fromIntegral $ [C.pure| int {KEY_AC_FORWARD}|]

keyACHome :: Int
keyACHome = fromIntegral $ [C.pure| int {KEY_AC_HOME} |]

keyACRefresh :: Int
keyACRefresh = fromIntegral $ [C.pure| int {KEY_AC_REFRESH} |]

keyACSearch :: Int
keyACSearch = fromIntegral $ [C.pure| int {KEY_AC_SEARCH} |]

keyACStop :: Int
keyACStop = fromIntegral $ [C.pure| int {KEY_AC_STOP} |]

keyAgain :: Int
keyAgain = fromIntegral $ [C.pure| int {KEY_AGAIN} |]

keyAltErase :: Int
keyAltErase = fromIntegral $ [C.pure| int {KEY_ALTERASE} |]

keyAmpersand :: Int
keyAmpersand = fromIntegral $ [C.pure| int {KEY_AMPERSAND} |]

keyAT :: Int
keyAT = fromIntegral $ [C.pure| int {KEY_AT} |]

keyAudioMute :: Int
keyAudioMute = fromIntegral $ [C.pure| int {KEY_AUDIOMUTE} |]

keyAudioNext :: Int
keyAudioNext = fromIntegral $ [C.pure| int {KEY_AUDIONEXT} |]

keyAudioPlay :: Int
keyAudioPlay = fromIntegral $ [C.pure| int {KEY_AUDIOPLAY} |]

keyAudioPrev :: Int
keyAudioPrev = fromIntegral $ [C.pure| int {KEY_AUDIOPREV} |]

keyAudioStop :: Int
keyAudioStop = fromIntegral $ [C.pure| int {KEY_AUDIOSTOP} |]

keyBackQuote :: Int
keyBackQuote = fromIntegral $ [C.pure| int {KEY_BACKQUOTE} |]

keyBackSlash :: Int
keyBackSlash = fromIntegral $ [C.pure| int {KEY_BACKSLASH} |]

keyBrightnessDown :: Int
keyBrightnessDown = fromIntegral $ [C.pure| int {KEY_BRIGHTNESSDOWN} |]

keyBrightnessUp :: Int
keyBrightnessUp = fromIntegral $ [C.pure| int {KEY_BRIGHTNESSUP} |]

keyCalculator :: Int
keyCalculator = fromIntegral $ [C.pure| int {KEY_CALCULATOR} |]

keyCancel :: Int
keyCancel = fromIntegral $ [C.pure| int {KEY_CANCEL} |]

keyCaret :: Int
keyCaret = fromIntegral $ [C.pure| int {KEY_CARET} |]

keyClear :: Int
keyClear = fromIntegral $ [C.pure| int {KEY_CLEAR} |]

keyClearAgain :: Int
keyClearAgain = fromIntegral $ [C.pure| int {KEY_CLEARAGAIN} |]

keyColon :: Int
keyColon = fromIntegral $ [C.pure| int {KEY_COLON} |]

keyComma :: Int
keyComma = fromIntegral $ [C.pure| int {KEY_COMMA} |]

keyComputer :: Int
keyComputer = fromIntegral $ [C.pure| int {KEY_COMPUTER} |]

keyCopy :: Int
keyCopy = fromIntegral $ [C.pure| int {KEY_COPY} |]

keyCRSel :: Int
keyCRSel = fromIntegral $ [C.pure| int {KEY_CRSEL} |]

keyCurrencySubUnit :: Int
keyCurrencySubUnit = fromIntegral $ [C.pure| int {KEY_CURRENCYSUBUNIT} |]

keyCurrencyUnit :: Int
keyCurrencyUnit = fromIntegral $ [C.pure| int {KEY_CURRENCYUNIT} |]

keyCut :: Int
keyCut = fromIntegral $ [C.pure| int {KEY_CUT} |]

keyDecimalSeparator :: Int
keyDecimalSeparator = fromIntegral $ [C.pure| int {KEY_DECIMALSEPARATOR} |]

keyDisplaySwitch :: Int
keyDisplaySwitch = fromIntegral $ [C.pure| int {KEY_DISPLAYSWITCH} |]

keyDollar :: Int
keyDollar = fromIntegral $ [C.pure| int {KEY_DOLLAR} |]

keyEject :: Int
keyEject = fromIntegral $ [C.pure| int {KEY_EJECT} |]

keyEquals :: Int
keyEquals = fromIntegral $ [C.pure| int {KEY_EQUALS} |]

keyExclaim :: Int
keyExclaim = fromIntegral $ [C.pure| int {KEY_EXCLAIM} |]

keyExsel :: Int
keyExsel = fromIntegral $ [C.pure| int {KEY_EXSEL} |]

keyFind :: Int
keyFind = fromIntegral $ [C.pure| int {KEY_FIND} |]

keyGreater :: Int
keyGreater = fromIntegral $ [C.pure| int {KEY_GREATER} |]

keyHash :: Int
keyHash = fromIntegral $ [C.pure| int {KEY_HASH} |]

keyHelp :: Int
keyHelp = fromIntegral $ [C.pure| int {KEY_HELP} |]

keyKBDillumDown :: Int
keyKBDillumDown = fromIntegral $ [C.pure| int {KEY_KBDILLUMDOWN} |]

keyKBDillumToggle :: Int
keyKBDillumToggle = fromIntegral $ [C.pure| int {KEY_KBDILLUMTOGGLE} |]

keyKBDillumUp :: Int
keyKBDillumUp = fromIntegral $ [C.pure| int {KEY_KBDILLUMUP} |]

keyKP00 :: Int
keyKP00 = fromIntegral $ [C.pure| int {KEY_KP_00} |]

keyKP000 :: Int
keyKP000 = fromIntegral $ [C.pure| int {KEY_KP_000} |]

keyKPA :: Int
keyKPA = fromIntegral $ [C.pure| int {KEY_KP_A} |]

keyKPAmpersand :: Int
keyKPAmpersand = fromIntegral $ [C.pure| int {KEY_KP_AMPERSAND} |]

keyKPAT :: Int
keyKPAT = fromIntegral $ [C.pure| int {KEY_KP_AT} |]

keyKPB :: Int
keyKPB = fromIntegral $ [C.pure| int {KEY_KP_B} |]

keyKPBackspace :: Int
keyKPBackspace = fromIntegral $ [C.pure| int {KEY_KP_BACKSPACE} |]

keyKPBinary :: Int
keyKPBinary = fromIntegral $ [C.pure| int {KEY_KP_BINARY} |]

keyKPC :: Int
keyKPC = fromIntegral $ [C.pure| int {KEY_KP_C} |]

keyKPClear :: Int
keyKPClear = fromIntegral $ [C.pure| int {KEY_KP_CLEAR} |]

keyKPClearEntry :: Int
keyKPClearEntry = fromIntegral $ [C.pure| int {KEY_KP_CLEARENTRY} |]

keyKPColon :: Int
keyKPColon = fromIntegral $ [C.pure| int {KEY_KP_COLON} |]

keyKPComma :: Int
keyKPComma = fromIntegral $ [C.pure| int {KEY_KP_COMMA} |]

keyKPD :: Int
keyKPD = fromIntegral $ [C.pure| int {KEY_KP_D} |]

keyKPDBLAmpersand :: Int
keyKPDBLAmpersand = fromIntegral $ [C.pure| int {KEY_KP_DBLAMPERSAND} |]

keyKPDBLVerticalBar :: Int
keyKPDBLVerticalBar = fromIntegral $ [C.pure| int {KEY_KP_DBLVERTICALBAR} |]

keyKPDecimal :: Int
keyKPDecimal = fromIntegral $ [C.pure| int {KEY_KP_DECIMAL} |]

keyKPE :: Int
keyKPE = fromIntegral $ [C.pure| int {KEY_KP_E} |]

keyKPEquals :: Int
keyKPEquals = fromIntegral $ [C.pure| int {KEY_KP_EQUALS} |]

keyKPEqualsAS400 :: Int
keyKPEqualsAS400 = fromIntegral $ [C.pure| int {KEY_KP_EQUALSAS400} |]

keyKPExclaim :: Int
keyKPExclaim = fromIntegral $ [C.pure| int {KEY_KP_EXCLAM} |]

keyKPF :: Int
keyKPF = fromIntegral $ [C.pure| int {KEY_KP_F} |]

keyKPGreater :: Int
keyKPGreater = fromIntegral $ [C.pure| int {KEY_KP_GREATER} |]

keyKPHash :: Int
keyKPHash = fromIntegral $ [C.pure| int {KEY_KP_HASH} |]

keyKPHexadecimal :: Int
keyKPHexadecimal = fromIntegral $ [C.pure| int {KEY_KP_HEXADECIMAL} |]

keyKPLeftBrace :: Int
keyKPLeftBrace = fromIntegral $ [C.pure| int {KEY_KP_LEFTBRACE} |]

keyKPLeftParen :: Int
keyKPLeftParen = fromIntegral $ [C.pure| int {KEY_KP_LEFTPAREN} |]

keyKPLess :: Int
keyKPLess = fromIntegral $ [C.pure| int {KEY_KP_LESS} |]

keyKPMemAdd :: Int
keyKPMemAdd = fromIntegral $ [C.pure| int {KEY_KP_MEMADD} |]

keyKPMemClear :: Int
keyKPMemClear = fromIntegral $ [C.pure| int {KEY_KP_MEMCLEAR} |]

keyKPMemDivide :: Int
keyKPMemDivide = fromIntegral $ [C.pure| int {KEY_KP_MEMDIVIDE} |]

keyKPMemMultiply :: Int
keyKPMemMultiply = fromIntegral $ [C.pure| int {KEY_KP_MEMMULTIPLY} |]

keyKPMemRecall :: Int
keyKPMemRecall = fromIntegral $ [C.pure| int {KEY_KP_MEMRECALL} |]

keyKPMemStore :: Int
keyKPMemStore = fromIntegral $ [C.pure| int {KEY_KP_MEMSTORE} |]

keyKPMemSubtract :: Int
keyKPMemSubtract = fromIntegral $ [C.pure| int {KEY_KP_MEMSUBTRACT} |]

keyKPOctal :: Int
keyKPOctal = fromIntegral $ [C.pure| int {KEY_KP_OCTAL} |]

keyKPPercent :: Int
keyKPPercent = fromIntegral $ [C.pure| int {KEY_KP_PERCENT} |]

keyKPPlusMinus :: Int
keyKPPlusMinus = fromIntegral $ [C.pure| int {KEY_KP_PLUSMINUS} |]

keyKPPower :: Int
keyKPPower = fromIntegral $ [C.pure| int {KEY_KP_POWER} |]

keyKPRightBrace :: Int
keyKPRightBrace = fromIntegral $ [C.pure| int {KEY_KP_RIGHTBRACE} |]

keyKPRightParen :: Int
keyKPRightParen = fromIntegral $ [C.pure| int {KEY_KP_RIGHTPAREN} |]

keyKPSpace :: Int
keyKPSpace = fromIntegral $ [C.pure| int {KEY_KP_SPACE} |]

keyKPTab :: Int
keyKPTab = fromIntegral $ [C.pure| int {KEY_KP_TAB} |]

keyKPVerticalBar :: Int
keyKPVerticalBar = fromIntegral $ [C.pure| int {KEY_KP_VERTICALBAR} |]

keyKPXor :: Int
keyKPXor = fromIntegral $ [C.pure| int {KEY_KP_XOR} |]

keyLeftBracket :: Int
keyLeftBracket = fromIntegral $ [C.pure| int {KEY_LEFTBRACKET} |]

keyLeftParen :: Int
keyLeftParen = fromIntegral $ [C.pure| int {KEY_LEFTPAREN} |]

keyLess :: Int
keyLess = fromIntegral $ [C.pure| int {KEY_LESS} |]

keyMail :: Int
keyMail = fromIntegral $ [C.pure| int {KEY_MAIL} |]

keyMediaSelect :: Int
keyMediaSelect = fromIntegral $ [C.pure| int {KEY_MEDIASELECT} |]

keyMenu :: Int
keyMenu = fromIntegral $ [C.pure| int {KEY_MENU} |]

keyMinus :: Int
keyMinus = fromIntegral $ [C.pure| int {KEY_MINUS} |]

keyMode :: Int
keyMode = fromIntegral $ [C.pure| int {KEY_MODE} |]

keyMute :: Int
keyMute = fromIntegral $ [C.pure| int {KEY_MUTE} |]

keyOper :: Int
keyOper = fromIntegral $ [C.pure| int {KEY_OPER} |]

keyOut :: Int
keyOut = fromIntegral $ [C.pure| int {KEY_OUT} |]

keyPaste :: Int
keyPaste = fromIntegral $ [C.pure| int {KEY_PASTE} |]

keyPercent :: Int
keyPercent = fromIntegral $ [C.pure| int {KEY_PERCENT} |]

keyPeriod :: Int
keyPeriod = fromIntegral $ [C.pure| int {KEY_PERIOD} |]

keyPlus :: Int
keyPlus = fromIntegral $ [C.pure| int {KEY_PLUS} |]

keyPower :: Int
keyPower = fromIntegral $ [C.pure| int {KEY_POWER} |]

keyPrior :: Int
keyPrior = fromIntegral $ [C.pure| int {KEY_PRIOR} |]

keyQuestion :: Int
keyQuestion = fromIntegral $ [C.pure| int {KEY_QUESTION} |]

keyQuote :: Int
keyQuote = fromIntegral $ [C.pure| int {KEY_QUOTE} |]

keyQuoteDbl :: Int
keyQuoteDbl = fromIntegral $ [C.pure| int {KEY_QUOTEDBL} |]

keyRightBracket :: Int
keyRightBracket = fromIntegral $ [C.pure| int {KEY_RIGHTBRACKET} |]

keyRightParen :: Int
keyRightParen = fromIntegral $ [C.pure| int {KEY_RIGHTPAREN} |]

keySemicolon :: Int
keySemicolon = fromIntegral $ [C.pure| int {KEY_SEMICOLON} |]

keySeparator :: Int
keySeparator = fromIntegral $ [C.pure| int {KEY_SEPARATOR} |]

keySlash :: Int
keySlash = fromIntegral $ [C.pure| int {KEY_SLASH} |]

keySleep :: Int
keySleep = fromIntegral $ [C.pure| int {KEY_SLEEP} |]

keyStop :: Int
keyStop = fromIntegral $ [C.pure| int {KEY_STOP} |]

keySysReq :: Int
keySysReq = fromIntegral $ [C.pure| int {KEY_SYSREQ} |]

keyThousandsSeparator :: Int
keyThousandsSeparator = fromIntegral $ [C.pure| int {KEY_THOUSANDSSEPARATOR} |]

keyUnderscore :: Int
keyUnderscore = fromIntegral $ [C.pure| int {KEY_UNDERSCORE} |]

keyUndo :: Int
keyUndo = fromIntegral $ [C.pure| int {KEY_UNDO} |]

keyVolumeDown :: Int
keyVolumeDown = fromIntegral $ [C.pure| int {KEY_VOLUMEDOWN} |]

keyVolumeUp :: Int
keyVolumeUp = fromIntegral $ [C.pure| int {KEY_VOLUMEUP} |]

keyWWW :: Int
keyWWW = fromIntegral $ [C.pure| int {KEY_WWW} |]

keyUnknown :: Int
keyUnknown = fromIntegral $ [C.pure| int {KEY_UNKNOWN}|]

data Scancode =
    ScancodeUnknown
  | ScancodeCtrl
  | ScancodeShift
  | ScancodeAlt
  | ScancodeGui
  | ScancodeA
  | ScancodeB
  | ScancodeC
  | ScancodeD
  | ScancodeE
  | ScancodeF
  | ScancodeG
  | ScancodeH
  | ScancodeI
  | ScancodeJ
  | ScancodeK
  | ScancodeL
  | ScancodeM
  | ScancodeN
  | ScancodeO
  | ScancodeP
  | ScancodeQ
  | ScancodeR
  | ScancodeS
  | ScancodeT
  | ScancodeU
  | ScancodeV
  | ScancodeW
  | ScancodeX
  | ScancodeY
  | ScancodeZ
  | Scancode1
  | Scancode2
  | Scancode3
  | Scancode4
  | Scancode5
  | Scancode6
  | Scancode7
  | Scancode8
  | Scancode9
  | Scancode0
  | ScancodeReturn
  | ScancodeEscape
  | ScancodeBackspace
  | ScancodeTab
  | ScancodeSpace
  | ScancodeMinus
  | ScancodeEquals
  | ScancodeLeftBracket
  | ScancodeRightBracket
  | ScancodeBackslash
  | ScancodeNonushash
  | ScancodeSemicolon
  | ScancodeApostrophe
  | ScancodeGrave
  | ScancodeComma
  | ScancodePeriod
  | ScancodeSlash
  | ScancodeCapslock
  | ScancodeF1
  | ScancodeF2
  | ScancodeF3
  | ScancodeF4
  | ScancodeF5
  | ScancodeF6
  | ScancodeF7
  | ScancodeF8
  | ScancodeF9
  | ScancodeF10
  | ScancodeF11
  | ScancodeF12
  | ScancodePrintScreen
  | ScancodeScrolLlock
  | ScancodePause
  | ScancodeInsert
  | ScancodeHome
  | ScancodePageUp
  | ScancodeDelete
  | ScancodeEnd
  | ScancodePageDown
  | ScancodeRight
  | ScancodeLeft
  | ScancodeDown
  | ScancodeUp
  | ScancodeNumLockClear
  | ScancodeKpDivide
  | ScancodeKpMultiply
  | ScancodeKpMinus
  | ScancodeKpPlus
  | ScancodeKpEnter
  | ScancodeKp1
  | ScancodeKp2
  | ScancodeKp3
  | ScancodeKp4
  | ScancodeKp5
  | ScancodeKp6
  | ScancodeKp7
  | ScancodeKp8
  | ScancodeKp9
  | ScancodeKp0
  | ScancodeKpPeriod
  | ScancodeNonusbackslash
  | ScancodeApplication
  | ScancodePower
  | ScancodeKpEquals
  | ScancodeF13
  | ScancodeF14
  | ScancodeF15
  | ScancodeF16
  | ScancodeF17
  | ScancodeF18
  | ScancodeF19
  | ScancodeF20
  | ScancodeF21
  | ScancodeF22
  | ScancodeF23
  | ScancodeF24
  | ScancodeExecute
  | ScancodeHelp
  | ScancodeMenu
  | ScancodeSelect
  | ScancodeStop
  | ScancodeAgain
  | ScancodeUndo
  | ScancodeCut
  | ScancodeCopy
  | ScancodePaste
  | ScancodeFind
  | ScancodeMute
  | ScancodeVolumeUp
  | ScancodeVolumeDown
  | ScancodeKpComma
  | ScancodeKpEqualsAs400
  | ScancodeInternational1
  | ScancodeInternational2
  | ScancodeInternational3
  | ScancodeInternational4
  | ScancodeInternational5
  | ScancodeInternational6
  | ScancodeInternational7
  | ScancodeInternational8
  | ScancodeInternational9
  | ScancodeLang1
  | ScancodeLang2
  | ScancodeLang3
  | ScancodeLang4
  | ScancodeLang5
  | ScancodeLang6
  | ScancodeLang7
  | ScancodeLang8
  | ScancodeLang9
  | ScancodeAltErase
  | ScancodeSysReq
  | ScancodeCancel
  | ScancodeClear
  | ScancodePrior
  | ScancodeReturn2
  | ScancodeSeparator
  | ScancodeOut
  | ScancodeOper
  | ScancodeClearAgain
  | ScancodeCrsel
  | ScancodeExsel
  | ScancodeKp00
  | ScancodeKp000
  | ScancodeThousandsSeparator
  | ScancodeDecimalSeparator
  | ScancodeCurrencyUnit
  | ScancodeCurrencySubUnit
  | ScancodeKpLeftParen
  | ScancodeKpRightParen
  | ScancodeKpLeftBrace
  | ScancodeKpRightBrace
  | ScancodeKpTab
  | ScancodeKpBackspace
  | ScancodeKpA
  | ScancodeKpB
  | ScancodeKpC
  | ScancodeKpD
  | ScancodeKpE
  | ScancodeKpF
  | ScancodeKpXor
  | ScancodeKpPower
  | ScancodeKpPercent
  | ScancodeKpLess
  | ScancodeKpGreater
  | ScancodeKpAmpersand
  | ScancodeKpDblAmpersand
  | ScancodeKpVerticalBar
  | ScancodeKpDblverticalBar
  | ScancodeKpColon
  | ScancodeKpHash
  | ScancodeKpSpace
  | ScancodeKpAt
  | ScancodeKpExclam
  | ScancodeKpMemStore
  | ScancodeKpMemRecall
  | ScancodeKpMemClear
  | ScancodeKpMemAdd
  | ScancodeKpMemSubtract
  | ScancodeKpMemMultiply
  | ScancodeKpMemDivide
  | ScancodeKpPlusMinus
  | ScancodeKpClear
  | ScancodeKpClearEntry
  | ScancodeKpBinary
  | ScancodeKpOctal
  | ScancodeKpDecimal
  | ScancodeKpHexadecimal
  | ScancodeLCtrl
  | ScancodeLShift
  | ScancodeLAlt
  | ScancodeLGui
  | ScancodeRCtrl
  | ScancodeRShift
  | ScancodeRAlt
  | ScancodeRGui
  | ScancodeMode
  | ScancodeAudioNext
  | ScancodeAudioPrev
  | ScancodeAudioStop
  | ScancodeAudioPlay
  | ScancodeAudioMute
  | ScancodeMediaSelect
  | ScancodeWww
  | ScancodeMail
  | ScancodeCalculator
  | ScancodeComputer
  | ScancodeAcSearch
  | ScancodeAcHome
  | ScancodeAcBack
  | ScancodeAcForward
  | ScancodeAcStop
  | ScancodeAcRefresh
  | ScancodeAcBookmarks
  | ScancodeBrightnessDown
  | ScancodeBrightnessUp
  | ScancodeDisplaySwitch
  | ScancodeKbdillumToggle
  | ScancodeKbdillumDown
  | ScancodeKbdillumUp
  | ScancodeEject
  | ScancodeSleep
  | ScancodeApp1
  | ScancodeApp2
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum Scancode where
  fromEnum = toUrhoScancode
  {-# INLINE fromEnum #-}
  toEnum = fromUrhoScancode
  {-# INLINE toEnum #-}

fromUrhoScancode :: Int -> Scancode
fromUrhoScancode i
  | i == scancodeUnknown = ScancodeUnknown
  | i == scancodeCtrl = ScancodeCtrl
  | i == scancodeShift = ScancodeShift
  | i == scancodeAlt = ScancodeAlt
  | i == scancodeGui = ScancodeGui
  | i == scancodeA = ScancodeA
  | i == scancodeB = ScancodeB
  | i == scancodeC = ScancodeC
  | i == scancodeD = ScancodeD
  | i == scancodeE = ScancodeE
  | i == scancodeF = ScancodeF
  | i == scancodeG = ScancodeG
  | i == scancodeH = ScancodeH
  | i == scancodeI = ScancodeI
  | i == scancodeJ = ScancodeJ
  | i == scancodeK = ScancodeK
  | i == scancodeL = ScancodeL
  | i == scancodeM = ScancodeM
  | i == scancodeN = ScancodeN
  | i == scancodeO = ScancodeO
  | i == scancodeP = ScancodeP
  | i == scancodeQ = ScancodeQ
  | i == scancodeR = ScancodeR
  | i == scancodeS = ScancodeS
  | i == scancodeT = ScancodeT
  | i == scancodeU = ScancodeU
  | i == scancodeV = ScancodeV
  | i == scancodeW = ScancodeW
  | i == scancodeX = ScancodeX
  | i == scancodeY = ScancodeY
  | i == scancodeZ = ScancodeZ
  | i == scancode1 = Scancode1
  | i == scancode2 = Scancode2
  | i == scancode3 = Scancode3
  | i == scancode4 = Scancode4
  | i == scancode5 = Scancode5
  | i == scancode6 = Scancode6
  | i == scancode7 = Scancode7
  | i == scancode8 = Scancode8
  | i == scancode9 = Scancode9
  | i == scancode0 = Scancode0
  | i == scancodeReturn = ScancodeReturn
  | i == scancodeEscape = ScancodeEscape
  | i == scancodeBackspace = ScancodeBackspace
  | i == scancodeTab = ScancodeTab
  | i == scancodeSpace = ScancodeSpace
  | i == scancodeMinus = ScancodeMinus
  | i == scancodeEquals = ScancodeEquals
  | i == scancodeLeftBracket = ScancodeLeftBracket
  | i == scancodeRightBracket = ScancodeRightBracket
  | i == scancodeBackslash = ScancodeBackslash
  | i == scancodeNonushash = ScancodeNonushash
  | i == scancodeSemicolon = ScancodeSemicolon
  | i == scancodeApostrophe = ScancodeApostrophe
  | i == scancodeGrave = ScancodeGrave
  | i == scancodeComma = ScancodeComma
  | i == scancodePeriod = ScancodePeriod
  | i == scancodeSlash = ScancodeSlash
  | i == scancodeCapslock = ScancodeCapslock
  | i == scancodeF1 = ScancodeF1
  | i == scancodeF2 = ScancodeF2
  | i == scancodeF3 = ScancodeF3
  | i == scancodeF4 = ScancodeF4
  | i == scancodeF5 = ScancodeF5
  | i == scancodeF6 = ScancodeF6
  | i == scancodeF7 = ScancodeF7
  | i == scancodeF8 = ScancodeF8
  | i == scancodeF9 = ScancodeF9
  | i == scancodeF10 = ScancodeF10
  | i == scancodeF11 = ScancodeF11
  | i == scancodeF12 = ScancodeF12
  | i == scancodePrintScreen = ScancodePrintScreen
  | i == scancodeScrolLlock = ScancodeScrolLlock
  | i == scancodePause = ScancodePause
  | i == scancodeInsert = ScancodeInsert
  | i == scancodeHome = ScancodeHome
  | i == scancodePageUp = ScancodePageUp
  | i == scancodeDelete = ScancodeDelete
  | i == scancodeEnd = ScancodeEnd
  | i == scancodePageDown = ScancodePageDown
  | i == scancodeRight = ScancodeRight
  | i == scancodeLeft = ScancodeLeft
  | i == scancodeDown = ScancodeDown
  | i == scancodeUp = ScancodeUp
  | i == scancodeNumLockClear = ScancodeNumLockClear
  | i == scancodeKpDivide = ScancodeKpDivide
  | i == scancodeKpMultiply = ScancodeKpMultiply
  | i == scancodeKpMinus = ScancodeKpMinus
  | i == scancodeKpPlus = ScancodeKpPlus
  | i == scancodeKpEnter = ScancodeKpEnter
  | i == scancodeKp1 = ScancodeKp1
  | i == scancodeKp2 = ScancodeKp2
  | i == scancodeKp3 = ScancodeKp3
  | i == scancodeKp4 = ScancodeKp4
  | i == scancodeKp5 = ScancodeKp5
  | i == scancodeKp6 = ScancodeKp6
  | i == scancodeKp7 = ScancodeKp7
  | i == scancodeKp8 = ScancodeKp8
  | i == scancodeKp9 = ScancodeKp9
  | i == scancodeKp0 = ScancodeKp0
  | i == scancodeKpPeriod = ScancodeKpPeriod
  | i == scancodeNonusbackslash = ScancodeNonusbackslash
  | i == scancodeApplication = ScancodeApplication
  | i == scancodePower = ScancodePower
  | i == scancodeKpEquals = ScancodeKpEquals
  | i == scancodeF13 = ScancodeF13
  | i == scancodeF14 = ScancodeF14
  | i == scancodeF15 = ScancodeF15
  | i == scancodeF16 = ScancodeF16
  | i == scancodeF17 = ScancodeF17
  | i == scancodeF18 = ScancodeF18
  | i == scancodeF19 = ScancodeF19
  | i == scancodeF20 = ScancodeF20
  | i == scancodeF21 = ScancodeF21
  | i == scancodeF22 = ScancodeF22
  | i == scancodeF23 = ScancodeF23
  | i == scancodeF24 = ScancodeF24
  | i == scancodeExecute = ScancodeExecute
  | i == scancodeHelp = ScancodeHelp
  | i == scancodeMenu = ScancodeMenu
  | i == scancodeSelect = ScancodeSelect
  | i == scancodeStop = ScancodeStop
  | i == scancodeAgain = ScancodeAgain
  | i == scancodeUndo = ScancodeUndo
  | i == scancodeCut = ScancodeCut
  | i == scancodeCopy = ScancodeCopy
  | i == scancodePaste = ScancodePaste
  | i == scancodeFind = ScancodeFind
  | i == scancodeMute = ScancodeMute
  | i == scancodeVolumeUp = ScancodeVolumeUp
  | i == scancodeVolumeDown = ScancodeVolumeDown
  | i == scancodeKpComma = ScancodeKpComma
  | i == scancodeKpEqualsAs400 = ScancodeKpEqualsAs400
  | i == scancodeInternational1 = ScancodeInternational1
  | i == scancodeInternational2 = ScancodeInternational2
  | i == scancodeInternational3 = ScancodeInternational3
  | i == scancodeInternational4 = ScancodeInternational4
  | i == scancodeInternational5 = ScancodeInternational5
  | i == scancodeInternational6 = ScancodeInternational6
  | i == scancodeInternational7 = ScancodeInternational7
  | i == scancodeInternational8 = ScancodeInternational8
  | i == scancodeInternational9 = ScancodeInternational9
  | i == scancodeLang1 = ScancodeLang1
  | i == scancodeLang2 = ScancodeLang2
  | i == scancodeLang3 = ScancodeLang3
  | i == scancodeLang4 = ScancodeLang4
  | i == scancodeLang5 = ScancodeLang5
  | i == scancodeLang6 = ScancodeLang6
  | i == scancodeLang7 = ScancodeLang7
  | i == scancodeLang8 = ScancodeLang8
  | i == scancodeLang9 = ScancodeLang9
  | i == scancodeAltErase = ScancodeAltErase
  | i == scancodeSysReq = ScancodeSysReq
  | i == scancodeCancel = ScancodeCancel
  | i == scancodeClear = ScancodeClear
  | i == scancodePrior = ScancodePrior
  | i == scancodeReturn2 = ScancodeReturn2
  | i == scancodeSeparator = ScancodeSeparator
  | i == scancodeOut = ScancodeOut
  | i == scancodeOper = ScancodeOper
  | i == scancodeClearAgain = ScancodeClearAgain
  | i == scancodeCrsel = ScancodeCrsel
  | i == scancodeExsel = ScancodeExsel
  | i == scancodeKp00 = ScancodeKp00
  | i == scancodeKp000 = ScancodeKp000
  | i == scancodeThousandsSeparator = ScancodeThousandsSeparator
  | i == scancodeDecimalSeparator = ScancodeDecimalSeparator
  | i == scancodeCurrencyUnit = ScancodeCurrencyUnit
  | i == scancodeCurrencySubUnit = ScancodeCurrencySubUnit
  | i == scancodeKpLeftParen = ScancodeKpLeftParen
  | i == scancodeKpRightParen = ScancodeKpRightParen
  | i == scancodeKpLeftBrace = ScancodeKpLeftBrace
  | i == scancodeKpRightBrace = ScancodeKpRightBrace
  | i == scancodeKpTab = ScancodeKpTab
  | i == scancodeKpBackspace = ScancodeKpBackspace
  | i == scancodeKpA = ScancodeKpA
  | i == scancodeKpB = ScancodeKpB
  | i == scancodeKpC = ScancodeKpC
  | i == scancodeKpD = ScancodeKpD
  | i == scancodeKpE = ScancodeKpE
  | i == scancodeKpF = ScancodeKpF
  | i == scancodeKpXor = ScancodeKpXor
  | i == scancodeKpPower = ScancodeKpPower
  | i == scancodeKpPercent = ScancodeKpPercent
  | i == scancodeKpLess = ScancodeKpLess
  | i == scancodeKpGreater = ScancodeKpGreater
  | i == scancodeKpAmpersand = ScancodeKpAmpersand
  | i == scancodeKpDblAmpersand = ScancodeKpDblAmpersand
  | i == scancodeKpVerticalBar = ScancodeKpVerticalBar
  | i == scancodeKpDblverticalBar = ScancodeKpDblverticalBar
  | i == scancodeKpColon = ScancodeKpColon
  | i == scancodeKpHash = ScancodeKpHash
  | i == scancodeKpSpace = ScancodeKpSpace
  | i == scancodeKpAt = ScancodeKpAt
  | i == scancodeKpExclam = ScancodeKpExclam
  | i == scancodeKpMemStore = ScancodeKpMemStore
  | i == scancodeKpMemRecall = ScancodeKpMemRecall
  | i == scancodeKpMemClear = ScancodeKpMemClear
  | i == scancodeKpMemAdd = ScancodeKpMemAdd
  | i == scancodeKpMemSubtract = ScancodeKpMemSubtract
  | i == scancodeKpMemMultiply = ScancodeKpMemMultiply
  | i == scancodeKpMemDivide = ScancodeKpMemDivide
  | i == scancodeKpPlusMinus = ScancodeKpPlusMinus
  | i == scancodeKpClear = ScancodeKpClear
  | i == scancodeKpClearEntry = ScancodeKpClearEntry
  | i == scancodeKpBinary = ScancodeKpBinary
  | i == scancodeKpOctal = ScancodeKpOctal
  | i == scancodeKpDecimal = ScancodeKpDecimal
  | i == scancodeKpHexadecimal = ScancodeKpHexadecimal
  | i == scancodeLCtrl = ScancodeLCtrl
  | i == scancodeLShift = ScancodeLShift
  | i == scancodeLAlt = ScancodeLAlt
  | i == scancodeLGui = ScancodeLGui
  | i == scancodeRCtrl = ScancodeRCtrl
  | i == scancodeRShift = ScancodeRShift
  | i == scancodeRAlt = ScancodeRAlt
  | i == scancodeRGui = ScancodeRGui
  | i == scancodeMode = ScancodeMode
  | i == scancodeAudioNext = ScancodeAudioNext
  | i == scancodeAudioPrev = ScancodeAudioPrev
  | i == scancodeAudioStop = ScancodeAudioStop
  | i == scancodeAudioPlay = ScancodeAudioPlay
  | i == scancodeAudioMute = ScancodeAudioMute
  | i == scancodeMediaSelect = ScancodeMediaSelect
  | i == scancodeWww = ScancodeWww
  | i == scancodeMail = ScancodeMail
  | i == scancodeCalculator = ScancodeCalculator
  | i == scancodeComputer = ScancodeComputer
  | i == scancodeAcSearch = ScancodeAcSearch
  | i == scancodeAcHome = ScancodeAcHome
  | i == scancodeAcBack = ScancodeAcBack
  | i == scancodeAcForward = ScancodeAcForward
  | i == scancodeAcStop = ScancodeAcStop
  | i == scancodeAcRefresh = ScancodeAcRefresh
  | i == scancodeAcBookmarks = ScancodeAcBookmarks
  | i == scancodeBrightnessDown = ScancodeBrightnessDown
  | i == scancodeBrightnessUp = ScancodeBrightnessUp
  | i == scancodeDisplaySwitch = ScancodeDisplaySwitch
  | i == scancodeKbdillumToggle = ScancodeKbdillumToggle
  | i == scancodeKbdillumDown = ScancodeKbdillumDown
  | i == scancodeKbdillumUp = ScancodeKbdillumUp
  | i == scancodeEject = ScancodeEject
  | i == scancodeSleep = ScancodeSleep
  | i == scancodeApp1 = ScancodeApp1
  | i == scancodeApp2 = ScancodeApp2
  | otherwise = ScancodeUnknown

toUrhoScancode :: Scancode -> Int
toUrhoScancode i = case i of
  ScancodeUnknown -> scancodeUnknown
  ScancodeCtrl -> scancodeCtrl
  ScancodeShift -> scancodeShift
  ScancodeAlt -> scancodeAlt
  ScancodeGui -> scancodeGui
  ScancodeA -> scancodeA
  ScancodeB -> scancodeB
  ScancodeC -> scancodeC
  ScancodeD -> scancodeD
  ScancodeE -> scancodeE
  ScancodeF -> scancodeF
  ScancodeG -> scancodeG
  ScancodeH -> scancodeH
  ScancodeI -> scancodeI
  ScancodeJ -> scancodeJ
  ScancodeK -> scancodeK
  ScancodeL -> scancodeL
  ScancodeM -> scancodeM
  ScancodeN -> scancodeN
  ScancodeO -> scancodeO
  ScancodeP -> scancodeP
  ScancodeQ -> scancodeQ
  ScancodeR -> scancodeR
  ScancodeS -> scancodeS
  ScancodeT -> scancodeT
  ScancodeU -> scancodeU
  ScancodeV -> scancodeV
  ScancodeW -> scancodeW
  ScancodeX -> scancodeX
  ScancodeY -> scancodeY
  ScancodeZ -> scancodeZ
  Scancode1 -> scancode1
  Scancode2 -> scancode2
  Scancode3 -> scancode3
  Scancode4 -> scancode4
  Scancode5 -> scancode5
  Scancode6 -> scancode6
  Scancode7 -> scancode7
  Scancode8 -> scancode8
  Scancode9 -> scancode9
  Scancode0 -> scancode0
  ScancodeReturn -> scancodeReturn
  ScancodeEscape -> scancodeEscape
  ScancodeBackspace -> scancodeBackspace
  ScancodeTab -> scancodeTab
  ScancodeSpace -> scancodeSpace
  ScancodeMinus -> scancodeMinus
  ScancodeEquals -> scancodeEquals
  ScancodeLeftBracket -> scancodeLeftBracket
  ScancodeRightBracket -> scancodeRightBracket
  ScancodeBackslash -> scancodeBackslash
  ScancodeNonushash -> scancodeNonushash
  ScancodeSemicolon -> scancodeSemicolon
  ScancodeApostrophe -> scancodeApostrophe
  ScancodeGrave -> scancodeGrave
  ScancodeComma -> scancodeComma
  ScancodePeriod -> scancodePeriod
  ScancodeSlash -> scancodeSlash
  ScancodeCapslock -> scancodeCapslock
  ScancodeF1 -> scancodeF1
  ScancodeF2 -> scancodeF2
  ScancodeF3 -> scancodeF3
  ScancodeF4 -> scancodeF4
  ScancodeF5 -> scancodeF5
  ScancodeF6 -> scancodeF6
  ScancodeF7 -> scancodeF7
  ScancodeF8 -> scancodeF8
  ScancodeF9 -> scancodeF9
  ScancodeF10 -> scancodeF10
  ScancodeF11 -> scancodeF11
  ScancodeF12 -> scancodeF12
  ScancodePrintScreen -> scancodePrintScreen
  ScancodeScrolLlock -> scancodeScrolLlock
  ScancodePause -> scancodePause
  ScancodeInsert -> scancodeInsert
  ScancodeHome -> scancodeHome
  ScancodePageUp -> scancodePageUp
  ScancodeDelete -> scancodeDelete
  ScancodeEnd -> scancodeEnd
  ScancodePageDown -> scancodePageDown
  ScancodeRight -> scancodeRight
  ScancodeLeft -> scancodeLeft
  ScancodeDown -> scancodeDown
  ScancodeUp -> scancodeUp
  ScancodeNumLockClear -> scancodeNumLockClear
  ScancodeKpDivide -> scancodeKpDivide
  ScancodeKpMultiply -> scancodeKpMultiply
  ScancodeKpMinus -> scancodeKpMinus
  ScancodeKpPlus -> scancodeKpPlus
  ScancodeKpEnter -> scancodeKpEnter
  ScancodeKp1 -> scancodeKp1
  ScancodeKp2 -> scancodeKp2
  ScancodeKp3 -> scancodeKp3
  ScancodeKp4 -> scancodeKp4
  ScancodeKp5 -> scancodeKp5
  ScancodeKp6 -> scancodeKp6
  ScancodeKp7 -> scancodeKp7
  ScancodeKp8 -> scancodeKp8
  ScancodeKp9 -> scancodeKp9
  ScancodeKp0 -> scancodeKp0
  ScancodeKpPeriod -> scancodeKpPeriod
  ScancodeNonusbackslash -> scancodeNonusbackslash
  ScancodeApplication -> scancodeApplication
  ScancodePower -> scancodePower
  ScancodeKpEquals -> scancodeKpEquals
  ScancodeF13 -> scancodeF13
  ScancodeF14 -> scancodeF14
  ScancodeF15 -> scancodeF15
  ScancodeF16 -> scancodeF16
  ScancodeF17 -> scancodeF17
  ScancodeF18 -> scancodeF18
  ScancodeF19 -> scancodeF19
  ScancodeF20 -> scancodeF20
  ScancodeF21 -> scancodeF21
  ScancodeF22 -> scancodeF22
  ScancodeF23 -> scancodeF23
  ScancodeF24 -> scancodeF24
  ScancodeExecute -> scancodeExecute
  ScancodeHelp -> scancodeHelp
  ScancodeMenu -> scancodeMenu
  ScancodeSelect -> scancodeSelect
  ScancodeStop -> scancodeStop
  ScancodeAgain -> scancodeAgain
  ScancodeUndo -> scancodeUndo
  ScancodeCut -> scancodeCut
  ScancodeCopy -> scancodeCopy
  ScancodePaste -> scancodePaste
  ScancodeFind -> scancodeFind
  ScancodeMute -> scancodeMute
  ScancodeVolumeUp -> scancodeVolumeUp
  ScancodeVolumeDown -> scancodeVolumeDown
  ScancodeKpComma -> scancodeKpComma
  ScancodeKpEqualsAs400 -> scancodeKpEqualsAs400
  ScancodeInternational1 -> scancodeInternational1
  ScancodeInternational2 -> scancodeInternational2
  ScancodeInternational3 -> scancodeInternational3
  ScancodeInternational4 -> scancodeInternational4
  ScancodeInternational5 -> scancodeInternational5
  ScancodeInternational6 -> scancodeInternational6
  ScancodeInternational7 -> scancodeInternational7
  ScancodeInternational8 -> scancodeInternational8
  ScancodeInternational9 -> scancodeInternational9
  ScancodeLang1 -> scancodeLang1
  ScancodeLang2 -> scancodeLang2
  ScancodeLang3 -> scancodeLang3
  ScancodeLang4 -> scancodeLang4
  ScancodeLang5 -> scancodeLang5
  ScancodeLang6 -> scancodeLang6
  ScancodeLang7 -> scancodeLang7
  ScancodeLang8 -> scancodeLang8
  ScancodeLang9 -> scancodeLang9
  ScancodeAltErase -> scancodeAltErase
  ScancodeSysReq -> scancodeSysReq
  ScancodeCancel -> scancodeCancel
  ScancodeClear -> scancodeClear
  ScancodePrior -> scancodePrior
  ScancodeReturn2 -> scancodeReturn2
  ScancodeSeparator -> scancodeSeparator
  ScancodeOut -> scancodeOut
  ScancodeOper -> scancodeOper
  ScancodeClearAgain -> scancodeClearAgain
  ScancodeCrsel -> scancodeCrsel
  ScancodeExsel -> scancodeExsel
  ScancodeKp00 -> scancodeKp00
  ScancodeKp000 -> scancodeKp000
  ScancodeThousandsSeparator -> scancodeThousandsSeparator
  ScancodeDecimalSeparator -> scancodeDecimalSeparator
  ScancodeCurrencyUnit -> scancodeCurrencyUnit
  ScancodeCurrencySubUnit -> scancodeCurrencySubUnit
  ScancodeKpLeftParen -> scancodeKpLeftParen
  ScancodeKpRightParen -> scancodeKpRightParen
  ScancodeKpLeftBrace -> scancodeKpLeftBrace
  ScancodeKpRightBrace -> scancodeKpRightBrace
  ScancodeKpTab -> scancodeKpTab
  ScancodeKpBackspace -> scancodeKpBackspace
  ScancodeKpA -> scancodeKpA
  ScancodeKpB -> scancodeKpB
  ScancodeKpC -> scancodeKpC
  ScancodeKpD -> scancodeKpD
  ScancodeKpE -> scancodeKpE
  ScancodeKpF -> scancodeKpF
  ScancodeKpXor -> scancodeKpXor
  ScancodeKpPower -> scancodeKpPower
  ScancodeKpPercent -> scancodeKpPercent
  ScancodeKpLess -> scancodeKpLess
  ScancodeKpGreater -> scancodeKpGreater
  ScancodeKpAmpersand -> scancodeKpAmpersand
  ScancodeKpDblAmpersand -> scancodeKpDblAmpersand
  ScancodeKpVerticalBar -> scancodeKpVerticalBar
  ScancodeKpDblverticalBar -> scancodeKpDblverticalBar
  ScancodeKpColon -> scancodeKpColon
  ScancodeKpHash -> scancodeKpHash
  ScancodeKpSpace -> scancodeKpSpace
  ScancodeKpAt -> scancodeKpAt
  ScancodeKpExclam -> scancodeKpExclam
  ScancodeKpMemStore -> scancodeKpMemStore
  ScancodeKpMemRecall -> scancodeKpMemRecall
  ScancodeKpMemClear -> scancodeKpMemClear
  ScancodeKpMemAdd -> scancodeKpMemAdd
  ScancodeKpMemSubtract -> scancodeKpMemSubtract
  ScancodeKpMemMultiply -> scancodeKpMemMultiply
  ScancodeKpMemDivide -> scancodeKpMemDivide
  ScancodeKpPlusMinus -> scancodeKpPlusMinus
  ScancodeKpClear -> scancodeKpClear
  ScancodeKpClearEntry -> scancodeKpClearEntry
  ScancodeKpBinary -> scancodeKpBinary
  ScancodeKpOctal -> scancodeKpOctal
  ScancodeKpDecimal -> scancodeKpDecimal
  ScancodeKpHexadecimal -> scancodeKpHexadecimal
  ScancodeLCtrl -> scancodeLCtrl
  ScancodeLShift -> scancodeLShift
  ScancodeLAlt -> scancodeLAlt
  ScancodeLGui -> scancodeLGui
  ScancodeRCtrl -> scancodeRCtrl
  ScancodeRShift -> scancodeRShift
  ScancodeRAlt -> scancodeRAlt
  ScancodeRGui -> scancodeRGui
  ScancodeMode -> scancodeMode
  ScancodeAudioNext -> scancodeAudioNext
  ScancodeAudioPrev -> scancodeAudioPrev
  ScancodeAudioStop -> scancodeAudioStop
  ScancodeAudioPlay -> scancodeAudioPlay
  ScancodeAudioMute -> scancodeAudioMute
  ScancodeMediaSelect -> scancodeMediaSelect
  ScancodeWww -> scancodeWww
  ScancodeMail -> scancodeMail
  ScancodeCalculator -> scancodeCalculator
  ScancodeComputer -> scancodeComputer
  ScancodeAcSearch -> scancodeAcSearch
  ScancodeAcHome -> scancodeAcHome
  ScancodeAcBack -> scancodeAcBack
  ScancodeAcForward -> scancodeAcForward
  ScancodeAcStop -> scancodeAcStop
  ScancodeAcRefresh -> scancodeAcRefresh
  ScancodeAcBookmarks -> scancodeAcBookmarks
  ScancodeBrightnessDown -> scancodeBrightnessDown
  ScancodeBrightnessUp -> scancodeBrightnessUp
  ScancodeDisplaySwitch -> scancodeDisplaySwitch
  ScancodeKbdillumToggle -> scancodeKbdillumToggle
  ScancodeKbdillumDown -> scancodeKbdillumDown
  ScancodeKbdillumUp -> scancodeKbdillumUp
  ScancodeEject -> scancodeEject
  ScancodeSleep -> scancodeSleep
  ScancodeApp1 -> scancodeApp1
  ScancodeApp2 -> scancodeApp2

scancodeUnknown :: Int
scancodeUnknown = fromIntegral $ [C.pure| int {SCANCODE_UNKNOWN} |]

scancodeCtrl :: Int
scancodeCtrl = fromIntegral $ [C.pure| int {SCANCODE_CTRL} |]

scancodeShift :: Int
scancodeShift = fromIntegral $ [C.pure| int {SCANCODE_SHIFT} |]

scancodeAlt :: Int
scancodeAlt = fromIntegral $ [C.pure| int {SCANCODE_ALT} |]

scancodeGui :: Int
scancodeGui = fromIntegral $ [C.pure| int {SCANCODE_GUI} |]

scancodeA :: Int
scancodeA = fromIntegral $ [C.pure| int {SCANCODE_A} |]

scancodeB :: Int
scancodeB = fromIntegral $ [C.pure| int {SCANCODE_B} |]

scancodeC :: Int
scancodeC = fromIntegral $ [C.pure| int {SCANCODE_C} |]

scancodeD :: Int
scancodeD = fromIntegral $ [C.pure| int {SCANCODE_D} |]

scancodeE :: Int
scancodeE = fromIntegral $ [C.pure| int {SCANCODE_E} |]

scancodeF :: Int
scancodeF = fromIntegral $ [C.pure| int {SCANCODE_F} |]

scancodeG :: Int
scancodeG = fromIntegral $ [C.pure| int {SCANCODE_G} |]

scancodeH :: Int
scancodeH = fromIntegral $ [C.pure| int {SCANCODE_H} |]

scancodeI :: Int
scancodeI = fromIntegral $ [C.pure| int {SCANCODE_I} |]

scancodeJ :: Int
scancodeJ = fromIntegral $ [C.pure| int {SCANCODE_J} |]

scancodeK :: Int
scancodeK = fromIntegral $ [C.pure| int {SCANCODE_K} |]

scancodeL :: Int
scancodeL = fromIntegral $ [C.pure| int {SCANCODE_L} |]

scancodeM :: Int
scancodeM = fromIntegral $ [C.pure| int {SCANCODE_M} |]

scancodeN :: Int
scancodeN = fromIntegral $ [C.pure| int {SCANCODE_N} |]

scancodeO :: Int
scancodeO = fromIntegral $ [C.pure| int {SCANCODE_O} |]

scancodeP :: Int
scancodeP = fromIntegral $ [C.pure| int {SCANCODE_P} |]

scancodeQ :: Int
scancodeQ = fromIntegral $ [C.pure| int {SCANCODE_Q} |]

scancodeR :: Int
scancodeR = fromIntegral $ [C.pure| int {SCANCODE_R} |]

scancodeS :: Int
scancodeS = fromIntegral $ [C.pure| int {SCANCODE_S} |]

scancodeT :: Int
scancodeT = fromIntegral $ [C.pure| int {SCANCODE_T} |]

scancodeU :: Int
scancodeU = fromIntegral $ [C.pure| int {SCANCODE_U} |]

scancodeV :: Int
scancodeV = fromIntegral $ [C.pure| int {SCANCODE_V} |]

scancodeW :: Int
scancodeW = fromIntegral $ [C.pure| int {SCANCODE_W} |]

scancodeX :: Int
scancodeX = fromIntegral $ [C.pure| int {SCANCODE_X} |]

scancodeY :: Int
scancodeY = fromIntegral $ [C.pure| int {SCANCODE_Y} |]

scancodeZ :: Int
scancodeZ = fromIntegral $ [C.pure| int {SCANCODE_Z} |]

scancode1 :: Int
scancode1 = fromIntegral $ [C.pure| int {SCANCODE_1} |]

scancode2 :: Int
scancode2 = fromIntegral $ [C.pure| int {SCANCODE_2} |]

scancode3 :: Int
scancode3 = fromIntegral $ [C.pure| int {SCANCODE_3} |]

scancode4 :: Int
scancode4 = fromIntegral $ [C.pure| int {SCANCODE_4} |]

scancode5 :: Int
scancode5 = fromIntegral $ [C.pure| int {SCANCODE_5} |]

scancode6 :: Int
scancode6 = fromIntegral $ [C.pure| int {SCANCODE_6} |]

scancode7 :: Int
scancode7 = fromIntegral $ [C.pure| int {SCANCODE_7} |]

scancode8 :: Int
scancode8 = fromIntegral $ [C.pure| int {SCANCODE_8} |]

scancode9 :: Int
scancode9 = fromIntegral $ [C.pure| int {SCANCODE_9} |]

scancode0 :: Int
scancode0 = fromIntegral $ [C.pure| int {SCANCODE_0} |]

scancodeReturn :: Int
scancodeReturn = fromIntegral $ [C.pure| int {SCANCODE_RETURN} |]

scancodeEscape :: Int
scancodeEscape = fromIntegral $ [C.pure| int {SCANCODE_ESCAPE} |]

scancodeBackspace :: Int
scancodeBackspace = fromIntegral $ [C.pure| int {SCANCODE_BACKSPACE} |]

scancodeTab :: Int
scancodeTab = fromIntegral $ [C.pure| int {SCANCODE_TAB} |]

scancodeSpace :: Int
scancodeSpace = fromIntegral $ [C.pure| int {SCANCODE_SPACE} |]

scancodeMinus :: Int
scancodeMinus = fromIntegral $ [C.pure| int {SCANCODE_MINUS} |]

scancodeEquals :: Int
scancodeEquals = fromIntegral $ [C.pure| int {SCANCODE_EQUALS} |]

scancodeLeftBracket :: Int
scancodeLeftBracket = fromIntegral $ [C.pure| int {SCANCODE_LEFTBRACKET} |]

scancodeRightBracket :: Int
scancodeRightBracket = fromIntegral $ [C.pure| int {SCANCODE_RIGHTBRACKET} |]

scancodeBackslash :: Int
scancodeBackslash = fromIntegral $ [C.pure| int {SCANCODE_BACKSLASH} |]

scancodeNonushash :: Int
scancodeNonushash = fromIntegral $ [C.pure| int {SCANCODE_NONUSHASH} |]

scancodeSemicolon :: Int
scancodeSemicolon = fromIntegral $ [C.pure| int {SCANCODE_SEMICOLON} |]

scancodeApostrophe :: Int
scancodeApostrophe = fromIntegral $ [C.pure| int {SCANCODE_APOSTROPHE} |]

scancodeGrave :: Int
scancodeGrave = fromIntegral $ [C.pure| int {SCANCODE_GRAVE} |]

scancodeComma :: Int
scancodeComma = fromIntegral $ [C.pure| int {SCANCODE_COMMA} |]

scancodePeriod :: Int
scancodePeriod = fromIntegral $ [C.pure| int {SCANCODE_PERIOD} |]

scancodeSlash :: Int
scancodeSlash = fromIntegral $ [C.pure| int {SCANCODE_SLASH} |]

scancodeCapslock :: Int
scancodeCapslock = fromIntegral $ [C.pure| int {SCANCODE_CAPSLOCK} |]

scancodeF1 :: Int
scancodeF1 = fromIntegral $ [C.pure| int {SCANCODE_F1} |]

scancodeF2 :: Int
scancodeF2 = fromIntegral $ [C.pure| int {SCANCODE_F2} |]

scancodeF3 :: Int
scancodeF3 = fromIntegral $ [C.pure| int {SCANCODE_F3} |]

scancodeF4 :: Int
scancodeF4 = fromIntegral $ [C.pure| int {SCANCODE_F4} |]

scancodeF5 :: Int
scancodeF5 = fromIntegral $ [C.pure| int {SCANCODE_F5} |]

scancodeF6 :: Int
scancodeF6 = fromIntegral $ [C.pure| int {SCANCODE_F6} |]

scancodeF7 :: Int
scancodeF7 = fromIntegral $ [C.pure| int {SCANCODE_F7} |]

scancodeF8 :: Int
scancodeF8 = fromIntegral $ [C.pure| int {SCANCODE_F8} |]

scancodeF9 :: Int
scancodeF9 = fromIntegral $ [C.pure| int {SCANCODE_F9} |]

scancodeF10 :: Int
scancodeF10 = fromIntegral $ [C.pure| int {SCANCODE_F10} |]

scancodeF11 :: Int
scancodeF11 = fromIntegral $ [C.pure| int {SCANCODE_F11} |]

scancodeF12 :: Int
scancodeF12 = fromIntegral $ [C.pure| int {SCANCODE_F12} |]

scancodePrintScreen :: Int
scancodePrintScreen = fromIntegral $ [C.pure| int {SCANCODE_PRINTSCREEN} |]

scancodeScrolLlock :: Int
scancodeScrolLlock = fromIntegral $ [C.pure| int {SCANCODE_SCROLLLOCK} |]

scancodePause :: Int
scancodePause = fromIntegral $ [C.pure| int {SCANCODE_PAUSE} |]

scancodeInsert :: Int
scancodeInsert = fromIntegral $ [C.pure| int {SCANCODE_INSERT} |]

scancodeHome :: Int
scancodeHome = fromIntegral $ [C.pure| int {SCANCODE_HOME} |]

scancodePageUp :: Int
scancodePageUp = fromIntegral $ [C.pure| int {SCANCODE_PAGEUP} |]

scancodeDelete :: Int
scancodeDelete = fromIntegral $ [C.pure| int {SCANCODE_DELETE} |]

scancodeEnd :: Int
scancodeEnd = fromIntegral $ [C.pure| int {SCANCODE_END} |]

scancodePageDown :: Int
scancodePageDown = fromIntegral $ [C.pure| int {SCANCODE_PAGEDOWN} |]

scancodeRight :: Int
scancodeRight = fromIntegral $ [C.pure| int {SCANCODE_RIGHT} |]

scancodeLeft :: Int
scancodeLeft = fromIntegral $ [C.pure| int {SCANCODE_LEFT} |]

scancodeDown :: Int
scancodeDown = fromIntegral $ [C.pure| int {SCANCODE_DOWN} |]

scancodeUp :: Int
scancodeUp = fromIntegral $ [C.pure| int {SCANCODE_UP} |]

scancodeNumLockClear :: Int
scancodeNumLockClear = fromIntegral $ [C.pure| int {SCANCODE_NUMLOCKCLEAR} |]

scancodeKpDivide :: Int
scancodeKpDivide = fromIntegral $ [C.pure| int {SCANCODE_KP_DIVIDE} |]

scancodeKpMultiply :: Int
scancodeKpMultiply = fromIntegral $ [C.pure| int {SCANCODE_KP_MULTIPLY} |]

scancodeKpMinus :: Int
scancodeKpMinus = fromIntegral $ [C.pure| int {SCANCODE_KP_MINUS} |]

scancodeKpPlus :: Int
scancodeKpPlus = fromIntegral $ [C.pure| int {SCANCODE_KP_PLUS} |]

scancodeKpEnter :: Int
scancodeKpEnter = fromIntegral $ [C.pure| int {SCANCODE_KP_ENTER} |]

scancodeKp1 :: Int
scancodeKp1 = fromIntegral $ [C.pure| int {SCANCODE_KP_1} |]

scancodeKp2 :: Int
scancodeKp2 = fromIntegral $ [C.pure| int {SCANCODE_KP_2} |]

scancodeKp3 :: Int
scancodeKp3 = fromIntegral $ [C.pure| int {SCANCODE_KP_3} |]

scancodeKp4 :: Int
scancodeKp4 = fromIntegral $ [C.pure| int {SCANCODE_KP_4} |]

scancodeKp5 :: Int
scancodeKp5 = fromIntegral $ [C.pure| int {SCANCODE_KP_5} |]

scancodeKp6 :: Int
scancodeKp6 = fromIntegral $ [C.pure| int {SCANCODE_KP_6} |]

scancodeKp7 :: Int
scancodeKp7 = fromIntegral $ [C.pure| int {SCANCODE_KP_7} |]

scancodeKp8 :: Int
scancodeKp8 = fromIntegral $ [C.pure| int {SCANCODE_KP_8} |]

scancodeKp9 :: Int
scancodeKp9 = fromIntegral $ [C.pure| int {SCANCODE_KP_9} |]

scancodeKp0 :: Int
scancodeKp0 = fromIntegral $ [C.pure| int {SCANCODE_KP_0} |]

scancodeKpPeriod :: Int
scancodeKpPeriod = fromIntegral $ [C.pure| int {SCANCODE_KP_PERIOD} |]

scancodeNonusbackslash :: Int
scancodeNonusbackslash = fromIntegral $ [C.pure| int {SCANCODE_NONUSBACKSLASH} |]

scancodeApplication :: Int
scancodeApplication = fromIntegral $ [C.pure| int {SCANCODE_APPLICATION} |]

scancodePower :: Int
scancodePower = fromIntegral $ [C.pure| int {SCANCODE_POWER} |]

scancodeKpEquals :: Int
scancodeKpEquals = fromIntegral $ [C.pure| int {SCANCODE_KP_EQUALS} |]

scancodeF13 :: Int
scancodeF13 = fromIntegral $ [C.pure| int {SCANCODE_F13} |]

scancodeF14 :: Int
scancodeF14 = fromIntegral $ [C.pure| int {SCANCODE_F14} |]

scancodeF15 :: Int
scancodeF15 = fromIntegral $ [C.pure| int {SCANCODE_F15} |]

scancodeF16 :: Int
scancodeF16 = fromIntegral $ [C.pure| int {SCANCODE_F16} |]

scancodeF17 :: Int
scancodeF17 = fromIntegral $ [C.pure| int {SCANCODE_F17} |]

scancodeF18 :: Int
scancodeF18 = fromIntegral $ [C.pure| int {SCANCODE_F18} |]

scancodeF19 :: Int
scancodeF19 = fromIntegral $ [C.pure| int {SCANCODE_F19} |]

scancodeF20 :: Int
scancodeF20 = fromIntegral $ [C.pure| int {SCANCODE_F20} |]

scancodeF21 :: Int
scancodeF21 = fromIntegral $ [C.pure| int {SCANCODE_F21} |]

scancodeF22 :: Int
scancodeF22 = fromIntegral $ [C.pure| int {SCANCODE_F22} |]

scancodeF23 :: Int
scancodeF23 = fromIntegral $ [C.pure| int {SCANCODE_F23} |]

scancodeF24 :: Int
scancodeF24 = fromIntegral $ [C.pure| int {SCANCODE_F24} |]

scancodeExecute :: Int
scancodeExecute = fromIntegral $ [C.pure| int {SCANCODE_EXECUTE} |]

scancodeHelp :: Int
scancodeHelp = fromIntegral $ [C.pure| int {SCANCODE_HELP} |]

scancodeMenu :: Int
scancodeMenu = fromIntegral $ [C.pure| int {SCANCODE_MENU} |]

scancodeSelect :: Int
scancodeSelect = fromIntegral $ [C.pure| int {SCANCODE_SELECT} |]

scancodeStop :: Int
scancodeStop = fromIntegral $ [C.pure| int {SCANCODE_STOP} |]

scancodeAgain :: Int
scancodeAgain = fromIntegral $ [C.pure| int {SCANCODE_AGAIN} |]

scancodeUndo :: Int
scancodeUndo = fromIntegral $ [C.pure| int {SCANCODE_UNDO} |]

scancodeCut :: Int
scancodeCut = fromIntegral $ [C.pure| int {SCANCODE_CUT} |]

scancodeCopy :: Int
scancodeCopy = fromIntegral $ [C.pure| int {SCANCODE_COPY} |]

scancodePaste :: Int
scancodePaste = fromIntegral $ [C.pure| int {SCANCODE_PASTE} |]

scancodeFind :: Int
scancodeFind = fromIntegral $ [C.pure| int {SCANCODE_FIND} |]

scancodeMute :: Int
scancodeMute = fromIntegral $ [C.pure| int {SCANCODE_MUTE} |]

scancodeVolumeUp :: Int
scancodeVolumeUp = fromIntegral $ [C.pure| int {SCANCODE_VOLUMEUP} |]

scancodeVolumeDown :: Int
scancodeVolumeDown = fromIntegral $ [C.pure| int {SCANCODE_VOLUMEDOWN} |]

scancodeKpComma :: Int
scancodeKpComma = fromIntegral $ [C.pure| int {SCANCODE_KP_COMMA} |]

scancodeKpEqualsAs400 :: Int
scancodeKpEqualsAs400 = fromIntegral $ [C.pure| int {SCANCODE_KP_EQUALSAS400} |]

scancodeInternational1 :: Int
scancodeInternational1 = fromIntegral $ [C.pure| int {SCANCODE_INTERNATIONAL1} |]

scancodeInternational2 :: Int
scancodeInternational2 = fromIntegral $ [C.pure| int {SCANCODE_INTERNATIONAL2} |]

scancodeInternational3 :: Int
scancodeInternational3 = fromIntegral $ [C.pure| int {SCANCODE_INTERNATIONAL3} |]

scancodeInternational4 :: Int
scancodeInternational4 = fromIntegral $ [C.pure| int {SCANCODE_INTERNATIONAL4} |]

scancodeInternational5 :: Int
scancodeInternational5 = fromIntegral $ [C.pure| int {SCANCODE_INTERNATIONAL5} |]

scancodeInternational6 :: Int
scancodeInternational6 = fromIntegral $ [C.pure| int {SCANCODE_INTERNATIONAL6} |]

scancodeInternational7 :: Int
scancodeInternational7 = fromIntegral $ [C.pure| int {SCANCODE_INTERNATIONAL7} |]

scancodeInternational8 :: Int
scancodeInternational8 = fromIntegral $ [C.pure| int {SCANCODE_INTERNATIONAL8} |]

scancodeInternational9 :: Int
scancodeInternational9 = fromIntegral $ [C.pure| int {SCANCODE_INTERNATIONAL9} |]

scancodeLang1 :: Int
scancodeLang1 = fromIntegral $ [C.pure| int {SCANCODE_LANG1} |]

scancodeLang2 :: Int
scancodeLang2 = fromIntegral $ [C.pure| int {SCANCODE_LANG2} |]

scancodeLang3 :: Int
scancodeLang3 = fromIntegral $ [C.pure| int {SCANCODE_LANG3} |]

scancodeLang4 :: Int
scancodeLang4 = fromIntegral $ [C.pure| int {SCANCODE_LANG4} |]

scancodeLang5 :: Int
scancodeLang5 = fromIntegral $ [C.pure| int {SCANCODE_LANG5} |]

scancodeLang6 :: Int
scancodeLang6 = fromIntegral $ [C.pure| int {SCANCODE_LANG6} |]

scancodeLang7 :: Int
scancodeLang7 = fromIntegral $ [C.pure| int {SCANCODE_LANG7} |]

scancodeLang8 :: Int
scancodeLang8 = fromIntegral $ [C.pure| int {SCANCODE_LANG8} |]

scancodeLang9 :: Int
scancodeLang9 = fromIntegral $ [C.pure| int {SCANCODE_LANG9} |]

scancodeAltErase :: Int
scancodeAltErase = fromIntegral $ [C.pure| int {SCANCODE_ALTERASE} |]

scancodeSysReq :: Int
scancodeSysReq = fromIntegral $ [C.pure| int {SCANCODE_SYSREQ} |]

scancodeCancel :: Int
scancodeCancel = fromIntegral $ [C.pure| int {SCANCODE_CANCEL} |]

scancodeClear :: Int
scancodeClear = fromIntegral $ [C.pure| int {SCANCODE_CLEAR} |]

scancodePrior :: Int
scancodePrior = fromIntegral $ [C.pure| int {SCANCODE_PRIOR} |]

scancodeReturn2 :: Int
scancodeReturn2 = fromIntegral $ [C.pure| int {SCANCODE_RETURN2} |]

scancodeSeparator :: Int
scancodeSeparator = fromIntegral $ [C.pure| int {SCANCODE_SEPARATOR} |]

scancodeOut :: Int
scancodeOut = fromIntegral $ [C.pure| int {SCANCODE_OUT} |]

scancodeOper :: Int
scancodeOper = fromIntegral $ [C.pure| int {SCANCODE_OPER} |]

scancodeClearAgain :: Int
scancodeClearAgain = fromIntegral $ [C.pure| int {SCANCODE_CLEARAGAIN} |]

scancodeCrsel :: Int
scancodeCrsel = fromIntegral $ [C.pure| int {SCANCODE_CRSEL} |]

scancodeExsel :: Int
scancodeExsel = fromIntegral $ [C.pure| int {SCANCODE_EXSEL} |]

scancodeKp00 :: Int
scancodeKp00 = fromIntegral $ [C.pure| int {SCANCODE_KP_00} |]

scancodeKp000 :: Int
scancodeKp000 = fromIntegral $ [C.pure| int {SCANCODE_KP_000} |]

scancodeThousandsSeparator :: Int
scancodeThousandsSeparator = fromIntegral $ [C.pure| int {SCANCODE_THOUSANDSSEPARATOR} |]

scancodeDecimalSeparator :: Int
scancodeDecimalSeparator = fromIntegral $ [C.pure| int {SCANCODE_DECIMALSEPARATOR} |]

scancodeCurrencyUnit :: Int
scancodeCurrencyUnit = fromIntegral $ [C.pure| int {SCANCODE_CURRENCYUNIT} |]

scancodeCurrencySubUnit :: Int
scancodeCurrencySubUnit = fromIntegral $ [C.pure| int {SCANCODE_CURRENCYSUBUNIT} |]

scancodeKpLeftParen :: Int
scancodeKpLeftParen = fromIntegral $ [C.pure| int {SCANCODE_KP_LEFTPAREN} |]

scancodeKpRightParen :: Int
scancodeKpRightParen = fromIntegral $ [C.pure| int {SCANCODE_KP_RIGHTPAREN} |]

scancodeKpLeftBrace :: Int
scancodeKpLeftBrace = fromIntegral $ [C.pure| int {SCANCODE_KP_LEFTBRACE} |]

scancodeKpRightBrace :: Int
scancodeKpRightBrace = fromIntegral $ [C.pure| int {SCANCODE_KP_RIGHTBRACE} |]

scancodeKpTab :: Int
scancodeKpTab = fromIntegral $ [C.pure| int {SCANCODE_KP_TAB} |]

scancodeKpBackspace :: Int
scancodeKpBackspace = fromIntegral $ [C.pure| int {SCANCODE_KP_BACKSPACE} |]

scancodeKpA :: Int
scancodeKpA = fromIntegral $ [C.pure| int {SCANCODE_KP_A} |]

scancodeKpB :: Int
scancodeKpB = fromIntegral $ [C.pure| int {SCANCODE_KP_B} |]

scancodeKpC :: Int
scancodeKpC = fromIntegral $ [C.pure| int {SCANCODE_KP_C} |]

scancodeKpD :: Int
scancodeKpD = fromIntegral $ [C.pure| int {SCANCODE_KP_D} |]

scancodeKpE :: Int
scancodeKpE = fromIntegral $ [C.pure| int {SCANCODE_KP_E} |]

scancodeKpF :: Int
scancodeKpF = fromIntegral $ [C.pure| int {SCANCODE_KP_F} |]

scancodeKpXor :: Int
scancodeKpXor = fromIntegral $ [C.pure| int {SCANCODE_KP_XOR} |]

scancodeKpPower :: Int
scancodeKpPower = fromIntegral $ [C.pure| int {SCANCODE_KP_POWER} |]

scancodeKpPercent :: Int
scancodeKpPercent = fromIntegral $ [C.pure| int {SCANCODE_KP_PERCENT} |]

scancodeKpLess :: Int
scancodeKpLess = fromIntegral $ [C.pure| int {SCANCODE_KP_LESS} |]

scancodeKpGreater :: Int
scancodeKpGreater = fromIntegral $ [C.pure| int {SCANCODE_KP_GREATER} |]

scancodeKpAmpersand :: Int
scancodeKpAmpersand = fromIntegral $ [C.pure| int {SCANCODE_KP_AMPERSAND} |]

scancodeKpDblAmpersand :: Int
scancodeKpDblAmpersand = fromIntegral $ [C.pure| int {SCANCODE_KP_DBLAMPERSAND} |]

scancodeKpVerticalBar :: Int
scancodeKpVerticalBar = fromIntegral $ [C.pure| int {SCANCODE_KP_VERTICALBAR} |]

scancodeKpDblverticalBar :: Int
scancodeKpDblverticalBar = fromIntegral $ [C.pure| int {SCANCODE_KP_DBLVERTICALBAR} |]

scancodeKpColon :: Int
scancodeKpColon = fromIntegral $ [C.pure| int {SCANCODE_KP_COLON} |]

scancodeKpHash :: Int
scancodeKpHash = fromIntegral $ [C.pure| int {SCANCODE_KP_HASH} |]

scancodeKpSpace :: Int
scancodeKpSpace = fromIntegral $ [C.pure| int {SCANCODE_KP_SPACE} |]

scancodeKpAt :: Int
scancodeKpAt = fromIntegral $ [C.pure| int {SCANCODE_KP_AT} |]

scancodeKpExclam :: Int
scancodeKpExclam = fromIntegral $ [C.pure| int {SCANCODE_KP_EXCLAM} |]

scancodeKpMemStore :: Int
scancodeKpMemStore = fromIntegral $ [C.pure| int {SCANCODE_KP_MEMSTORE} |]

scancodeKpMemRecall :: Int
scancodeKpMemRecall = fromIntegral $ [C.pure| int {SCANCODE_KP_MEMRECALL} |]

scancodeKpMemClear :: Int
scancodeKpMemClear = fromIntegral $ [C.pure| int {SCANCODE_KP_MEMCLEAR} |]

scancodeKpMemAdd :: Int
scancodeKpMemAdd = fromIntegral $ [C.pure| int {SCANCODE_KP_MEMADD} |]

scancodeKpMemSubtract :: Int
scancodeKpMemSubtract = fromIntegral $ [C.pure| int {SCANCODE_KP_MEMSUBTRACT} |]

scancodeKpMemMultiply :: Int
scancodeKpMemMultiply = fromIntegral $ [C.pure| int {SCANCODE_KP_MEMMULTIPLY} |]

scancodeKpMemDivide :: Int
scancodeKpMemDivide = fromIntegral $ [C.pure| int {SCANCODE_KP_MEMDIVIDE} |]

scancodeKpPlusMinus :: Int
scancodeKpPlusMinus = fromIntegral $ [C.pure| int {SCANCODE_KP_PLUSMINUS} |]

scancodeKpClear :: Int
scancodeKpClear = fromIntegral $ [C.pure| int {SCANCODE_KP_CLEAR} |]

scancodeKpClearEntry :: Int
scancodeKpClearEntry = fromIntegral $ [C.pure| int {SCANCODE_KP_CLEARENTRY} |]

scancodeKpBinary :: Int
scancodeKpBinary = fromIntegral $ [C.pure| int {SCANCODE_KP_BINARY} |]

scancodeKpOctal :: Int
scancodeKpOctal = fromIntegral $ [C.pure| int {SCANCODE_KP_OCTAL} |]

scancodeKpDecimal :: Int
scancodeKpDecimal = fromIntegral $ [C.pure| int {SCANCODE_KP_DECIMAL} |]

scancodeKpHexadecimal :: Int
scancodeKpHexadecimal = fromIntegral $ [C.pure| int {SCANCODE_KP_HEXADECIMAL} |]

scancodeLCtrl :: Int
scancodeLCtrl = fromIntegral $ [C.pure| int {SCANCODE_LCTRL} |]

scancodeLShift :: Int
scancodeLShift = fromIntegral $ [C.pure| int {SCANCODE_LSHIFT} |]

scancodeLAlt :: Int
scancodeLAlt = fromIntegral $ [C.pure| int {SCANCODE_LALT} |]

scancodeLGui :: Int
scancodeLGui = fromIntegral $ [C.pure| int {SCANCODE_LGUI} |]

scancodeRCtrl :: Int
scancodeRCtrl = fromIntegral $ [C.pure| int {SCANCODE_RCTRL} |]

scancodeRShift :: Int
scancodeRShift = fromIntegral $ [C.pure| int {SCANCODE_RSHIFT} |]

scancodeRAlt :: Int
scancodeRAlt = fromIntegral $ [C.pure| int {SCANCODE_RALT} |]

scancodeRGui :: Int
scancodeRGui = fromIntegral $ [C.pure| int {SCANCODE_RGUI} |]

scancodeMode :: Int
scancodeMode = fromIntegral $ [C.pure| int {SCANCODE_MODE} |]

scancodeAudioNext :: Int
scancodeAudioNext = fromIntegral $ [C.pure| int {SCANCODE_AUDIONEXT} |]

scancodeAudioPrev :: Int
scancodeAudioPrev = fromIntegral $ [C.pure| int {SCANCODE_AUDIOPREV} |]

scancodeAudioStop :: Int
scancodeAudioStop = fromIntegral $ [C.pure| int {SCANCODE_AUDIOSTOP} |]

scancodeAudioPlay :: Int
scancodeAudioPlay = fromIntegral $ [C.pure| int {SCANCODE_AUDIOPLAY} |]

scancodeAudioMute :: Int
scancodeAudioMute = fromIntegral $ [C.pure| int {SCANCODE_AUDIOMUTE} |]

scancodeMediaSelect :: Int
scancodeMediaSelect = fromIntegral $ [C.pure| int {SCANCODE_MEDIASELECT} |]

scancodeWww :: Int
scancodeWww = fromIntegral $ [C.pure| int {SCANCODE_WWW} |]

scancodeMail :: Int
scancodeMail = fromIntegral $ [C.pure| int {SCANCODE_MAIL} |]

scancodeCalculator :: Int
scancodeCalculator = fromIntegral $ [C.pure| int {SCANCODE_CALCULATOR} |]

scancodeComputer :: Int
scancodeComputer = fromIntegral $ [C.pure| int {SCANCODE_COMPUTER} |]

scancodeAcSearch :: Int
scancodeAcSearch = fromIntegral $ [C.pure| int {SCANCODE_AC_SEARCH} |]

scancodeAcHome :: Int
scancodeAcHome = fromIntegral $ [C.pure| int {SCANCODE_AC_HOME} |]

scancodeAcBack :: Int
scancodeAcBack = fromIntegral $ [C.pure| int {SCANCODE_AC_BACK} |]

scancodeAcForward :: Int
scancodeAcForward = fromIntegral $ [C.pure| int {SCANCODE_AC_FORWARD} |]

scancodeAcStop :: Int
scancodeAcStop = fromIntegral $ [C.pure| int {SCANCODE_AC_STOP} |]

scancodeAcRefresh :: Int
scancodeAcRefresh = fromIntegral $ [C.pure| int {SCANCODE_AC_REFRESH} |]

scancodeAcBookmarks :: Int
scancodeAcBookmarks = fromIntegral $ [C.pure| int {SCANCODE_AC_BOOKMARKS} |]

scancodeBrightnessDown :: Int
scancodeBrightnessDown = fromIntegral $ [C.pure| int {SCANCODE_BRIGHTNESSDOWN} |]

scancodeBrightnessUp :: Int
scancodeBrightnessUp = fromIntegral $ [C.pure| int {SCANCODE_BRIGHTNESSUP} |]

scancodeDisplaySwitch :: Int
scancodeDisplaySwitch = fromIntegral $ [C.pure| int {SCANCODE_DISPLAYSWITCH} |]

scancodeKbdillumToggle :: Int
scancodeKbdillumToggle = fromIntegral $ [C.pure| int {SCANCODE_KBDILLUMTOGGLE} |]

scancodeKbdillumDown :: Int
scancodeKbdillumDown = fromIntegral $ [C.pure| int {SCANCODE_KBDILLUMDOWN} |]

scancodeKbdillumUp :: Int
scancodeKbdillumUp = fromIntegral $ [C.pure| int {SCANCODE_KBDILLUMUP} |]

scancodeEject :: Int
scancodeEject = fromIntegral $ [C.pure| int {SCANCODE_EJECT} |]

scancodeSleep :: Int
scancodeSleep = fromIntegral $ [C.pure| int {SCANCODE_SLEEP} |]

scancodeApp1 :: Int
scancodeApp1 = fromIntegral $ [C.pure| int {SCANCODE_APP1} |]

scancodeApp2 :: Int
scancodeApp2 = fromIntegral $ [C.pure| int {SCANCODE_APP2} |]

data HatPosition =
    HatCenter
  | HatUp
  | HatRight
  | HatDown
  | HatLeft
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum HatPosition where
  fromEnum = fromUrhoHatPosition
  {-# INLINE fromEnum #-}
  toEnum = toUrhoHatPosition
  {-# INLINE toEnum #-}

toUrhoHatPosition :: Int -> HatPosition
toUrhoHatPosition i
  | i == hatCenter = HatCenter
  | i == hatUp = HatUp
  | i == hatRight = HatRight
  | i == hatDown = HatDown
  | i == hatLeft = HatLeft
  | otherwise = HatCenter

fromUrhoHatPosition :: HatPosition -> Int
fromUrhoHatPosition i = case i of
  HatCenter -> hatCenter
  HatUp -> hatUp
  HatRight -> hatRight
  HatDown -> hatDown
  HatLeft -> hatLeft

hatCenter :: Int
hatCenter = fromIntegral $ [C.pure| int {HAT_CENTER} |]

hatUp :: Int
hatUp = fromIntegral $ [C.pure| int {HAT_UP} |]

hatRight :: Int
hatRight = fromIntegral $ [C.pure| int {HAT_RIGHT} |]

hatDown :: Int
hatDown = fromIntegral $ [C.pure| int {HAT_DOWN} |]

hatLeft :: Int
hatLeft = fromIntegral $ [C.pure| int {HAT_LEFT} |]

data ControllerButton =
    ControllerButtonA
  | ControllerButtonB
  | ControllerButtonX
  | ControllerButtonY
  | ControllerButtonBack
  | ControllerButtonGuide
  | ControllerButtonStart
  | ControllerButtonLeftStick
  | ControllerButtonRightStick
  | ControllerButtonLeftShoulder
  | ControllerButtonRightShoulder
  | ControllerButtonDpadUp
  | ControllerButtonDpadDown
  | ControllerButtonDpadLeft
  | ControllerButtonDpadRight
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum ControllerButton where
  fromEnum = fromUrhoControllerButton
  {-# INLINE fromEnum #-}
  toEnum = toUrhoControllerButton
  {-# INLINE toEnum #-}

toUrhoControllerButton :: Int -> ControllerButton
toUrhoControllerButton i
  | i == controllerButtonA = ControllerButtonA
  | i == controllerButtonB = ControllerButtonB
  | i == controllerButtonX = ControllerButtonX
  | i == controllerButtonY = ControllerButtonY
  | i == controllerButtonBack = ControllerButtonBack
  | i == controllerButtonGuide = ControllerButtonGuide
  | i == controllerButtonStart = ControllerButtonStart
  | i == controllerButtonLeftstick = ControllerButtonLeftStick
  | i == controllerButtonRightstick = ControllerButtonRightStick
  | i == controllerButtonLeftshoulder = ControllerButtonLeftShoulder
  | i == controllerButtonRightshoulder = ControllerButtonRightShoulder
  | i == controllerButtonDpadUp = ControllerButtonDpadUp
  | i == controllerButtonDpadDown = ControllerButtonDpadDown
  | i == controllerButtonDpadLeft = ControllerButtonDpadLeft
  | i == controllerButtonDpadRight = ControllerButtonDpadRight
  | otherwise = ControllerButtonA

fromUrhoControllerButton :: ControllerButton -> Int
fromUrhoControllerButton i = case i of
  ControllerButtonA -> controllerButtonA
  ControllerButtonB -> controllerButtonB
  ControllerButtonX -> controllerButtonX
  ControllerButtonY -> controllerButtonY
  ControllerButtonBack -> controllerButtonBack
  ControllerButtonGuide -> controllerButtonGuide
  ControllerButtonStart -> controllerButtonStart
  ControllerButtonLeftStick -> controllerButtonLeftstick
  ControllerButtonRightStick -> controllerButtonRightstick
  ControllerButtonLeftShoulder -> controllerButtonLeftshoulder
  ControllerButtonRightShoulder -> controllerButtonRightshoulder
  ControllerButtonDpadUp -> controllerButtonDpadUp
  ControllerButtonDpadDown -> controllerButtonDpadDown
  ControllerButtonDpadLeft -> controllerButtonDpadLeft
  ControllerButtonDpadRight -> controllerButtonDpadRight

controllerButtonA :: Int
controllerButtonA = fromIntegral [C.pure| int { CONTROLLER_BUTTON_A } |]

controllerButtonB :: Int
controllerButtonB = fromIntegral [C.pure| int { CONTROLLER_BUTTON_B } |]

controllerButtonX :: Int
controllerButtonX = fromIntegral [C.pure| int { CONTROLLER_BUTTON_X } |]

controllerButtonY :: Int
controllerButtonY = fromIntegral [C.pure| int { CONTROLLER_BUTTON_Y } |]

controllerButtonBack :: Int
controllerButtonBack = fromIntegral [C.pure| int { CONTROLLER_BUTTON_BACK } |]

controllerButtonGuide :: Int
controllerButtonGuide = fromIntegral [C.pure| int { CONTROLLER_BUTTON_GUIDE } |]

controllerButtonStart :: Int
controllerButtonStart = fromIntegral [C.pure| int { CONTROLLER_BUTTON_START } |]

controllerButtonLeftstick :: Int
controllerButtonLeftstick = fromIntegral [C.pure| int { CONTROLLER_BUTTON_LEFTSTICK } |]

controllerButtonRightstick :: Int
controllerButtonRightstick = fromIntegral [C.pure| int { CONTROLLER_BUTTON_RIGHTSTICK } |]

controllerButtonLeftshoulder :: Int
controllerButtonLeftshoulder = fromIntegral [C.pure| int { CONTROLLER_BUTTON_LEFTSHOULDER } |]

controllerButtonRightshoulder :: Int
controllerButtonRightshoulder = fromIntegral [C.pure| int { CONTROLLER_BUTTON_RIGHTSHOULDER } |]

controllerButtonDpadUp :: Int
controllerButtonDpadUp = fromIntegral [C.pure| int { CONTROLLER_BUTTON_DPAD_UP } |]

controllerButtonDpadDown :: Int
controllerButtonDpadDown = fromIntegral [C.pure| int { CONTROLLER_BUTTON_DPAD_DOWN } |]

controllerButtonDpadLeft :: Int
controllerButtonDpadLeft = fromIntegral [C.pure| int { CONTROLLER_BUTTON_DPAD_LEFT } |]

controllerButtonDpadRight :: Int
controllerButtonDpadRight = fromIntegral [C.pure| int { CONTROLLER_BUTTON_DPAD_RIGHT } |]

data ControllerAxis =
    ControllerAxisLeftX
  | ControllerAxisLeftY
  | ControllerAxisRightX
  | ControllerAxisRightY
  | ControllerAxisTriggerLeft
  | ControllerAxisTriggerRight
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum ControllerAxis where
  fromEnum = fromUrhoControllerAxis
  {-# INLINE fromEnum #-}
  toEnum = toUrhoControllerAxis
  {-# INLINE toEnum #-}

toUrhoControllerAxis :: Int -> ControllerAxis
toUrhoControllerAxis i
  | i == controllerAxisLeftx = ControllerAxisLeftX
  | i == controllerAxisLefty = ControllerAxisLeftY
  | i == controllerAxisRightx = ControllerAxisRightX
  | i == controllerAxisRighty = ControllerAxisRightY
  | i == controllerAxisTriggerleft = ControllerAxisTriggerLeft
  | i == controllerAxisTriggerright = ControllerAxisTriggerRight
  | otherwise = ControllerAxisLeftX

fromUrhoControllerAxis :: ControllerAxis -> Int
fromUrhoControllerAxis i = case i of
  ControllerAxisLeftX -> controllerAxisLeftx
  ControllerAxisLeftY -> controllerAxisLefty
  ControllerAxisRightX -> controllerAxisRightx
  ControllerAxisRightY -> controllerAxisRighty
  ControllerAxisTriggerLeft -> controllerAxisTriggerleft
  ControllerAxisTriggerRight -> controllerAxisTriggerright

controllerAxisLeftx :: Int
controllerAxisLeftx = fromIntegral [C.pure| int {CONTROLLER_AXIS_LEFTX} |]

controllerAxisLefty :: Int
controllerAxisLefty = fromIntegral [C.pure| int {CONTROLLER_AXIS_LEFTY} |]

controllerAxisRightx :: Int
controllerAxisRightx = fromIntegral [C.pure| int {CONTROLLER_AXIS_RIGHTX} |]

controllerAxisRighty :: Int
controllerAxisRighty = fromIntegral [C.pure| int {CONTROLLER_AXIS_RIGHTY} |]

controllerAxisTriggerleft :: Int
controllerAxisTriggerleft = fromIntegral [C.pure| int {CONTROLLER_AXIS_TRIGGERLEFT} |]

controllerAxisTriggerright :: Int
controllerAxisTriggerright = fromIntegral [C.pure| int {CONTROLLER_AXIS_TRIGGERRIGHT} |]
