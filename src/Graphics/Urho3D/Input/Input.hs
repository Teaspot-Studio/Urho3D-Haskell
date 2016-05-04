{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Input.Input(
    Input
  , MouseMode(..)
  -- * Touch State
  , TouchState
  , touchedElement 
  , touchID 
  , touchPosition 
  , touchLastPosition 
  , touchDelta 
  , touchPressure
  -- * Joystick State
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
  , JoystickID
  , Joystick
  , GameController
  , SDL_Joystick
  , SDL_GameController
  , toSDLJoystick
  , toSDLController
  , fromSDLJoystick
  , fromSDLController
  -- * Input API
  , inputContext
  , mousePoistionOffscreen
  -- ** Input methods
  , inputUpdate
  , inputSetToggleFullscreen
  , inputSetMouseVisible
  , inputResetMouseVisible
  , inputSetMouseGrabbed
  , inputResetMouseGrabbed
  , inputSetMouseMode
  , inputResetMouseMode
  , inputAddScreenJoystick
  , inputRemoveScreenJoystick
  , inputSetScreenJoystickVisible
  , inputSetScreenKeyboardVisible
  , inputSetTouchEmulation
  , inputRecordGesture
  , inputSaveGestures
  , inputSaveGesture
  , inputLoadGestures
  , inputRemoveGesture
  , inputRemoveAllGestures
  , inputGetKeyFromName
  , inputGetKeyFromScancode
  , inputGetKeyName
  , inputGetScancodeFromKey
  , inputGetScancodeFromName
  , inputGetScancodeName
  , inputGetKeyDown
  , inputGetKeyPress
  , inputGetScancodeDown
  , inputGetScancodePress
  , inputGetMouseButtonDown
  , inputGetMouseButtonPress
  , inputGetQualifierDown
  , inputGetQualifierPress
  , inputGetQualifiers
  , inputGetMousePosition
  , inputGetMouseMove
  , inputGetMouseMoveX
  , inputGetMouseMoveY
  , inputGetMouseMoveWheel
  , inputGetNumTouches
  , inputGetTouch
  , inputGetNumJoysticks
  , inputGetJoystick
  , inputGetJoystickByIndex
  , inputGetJoystickByName
  , inputGetToggleFullscreen
  , inputIsScreenJoystickVisible
  , inputGetScreenKeyboardSupport
  , inputIsScreenKeyboardVisible
  , inputGetTouchEmulation
  , inputIsMouseVisible
  , inputIsMouseGrabbed
  , inputIsMouseLocked
  , inputGetMouseMode
  , inputHasFocus
  , inputIsMinimized
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Input.Internal.Input
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Resource.XMLFile
import Graphics.Urho3D.Math.Vector2 
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.IO.Deserializer
import Graphics.Urho3D.IO.Serializer
import Data.Monoid
import Foreign 
import Foreign.C.String
import Text.RawString.QQ
import Control.Lens 
import System.IO.Unsafe (unsafePerformIO)
import Graphics.Urho3D.Input.Events

C.context (C.cppCtx 
  <> inputCntx 
  <> objectContext 
  <> xmlFileContext 
  <> vector2Context 
  <> uiElementContext 
  <> vectorContext
  <> serializerContext
  <> deserializerContext)

C.include "<Urho3D/Input/Input.h>"
C.using "namespace Urho3D"

C.verbatim "typedef WeakPtr<UIElement> WeakUIElement;"
C.verbatim "typedef PODVector<bool> PODVectorBool;"
C.verbatim "typedef PODVector<float> PODVectorFloat;"
C.verbatim "typedef PODVector<int> PODVectorInt;"

inputContext :: C.Context 
inputContext = objectContext <> inputCntx <> vector2Context

deriveParent ''Object ''Input

instance Subsystem Input where 
  getSubsystemImpl ptr = [C.exp| Input* { $(Object* ptr)->GetSubsystem<Input>() } |]

C.verbatim [r|
template <class T>
class Traits
{
public:
    struct AlignmentFinder
    {
      char a; 
      T b;
    };

    enum {AlignmentOf = sizeof(AlignmentFinder) - sizeof(T)};
};
|]

instance Storable TouchState where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(TouchState) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<TouchState>::AlignmentOf } |]
  peek ptr = do 
    tid <- fromIntegral <$> [C.exp| int { $(TouchState* ptr)->touchID_ } |]
    pv <- peek =<< [C.exp| IntVector2* { &$(TouchState* ptr)->position_ } |]
    lpv <- peek =<< [C.exp| IntVector2* { &$(TouchState* ptr)->lastPosition_ } |]
    dv <- peek =<< [C.exp| IntVector2* { &$(TouchState* ptr)->delta_ } |]
    pr <- realToFrac <$> [C.exp| float { $(TouchState* ptr)->pressure_ } |]
    e <- peekWeakPtr =<< [C.exp| WeakUIElement* { new WeakPtr<UIElement>($(TouchState* ptr)->touchedElement_) }|]
    return $ TouchState e tid pv lpv dv pr 

  poke ptr ts = do 
    with (ts ^. touchPosition) $ \pv -> 
      with (ts ^. touchLastPosition) $ \lpv -> 
        with (ts ^. touchDelta) $ \dv -> do
          let tid = fromIntegral $ ts ^. touchID 
              pr = realToFrac $ ts ^. touchPressure
              e = parentPointer $ ts ^. touchedElement
          [C.block| void { 
            $(TouchState* ptr)->touchID_ = $(float tid);
            $(TouchState* ptr)->position_ = *$(IntVector2* pv);
            $(TouchState* ptr)->lastPosition_ = *$(IntVector2* lpv);
            $(TouchState* ptr)->delta_ = *$(IntVector2* dv);
            $(TouchState* ptr)->pressure_ = $(float pr);
            $(TouchState* ptr)->touchedElement_ = WeakPtr<UIElement>($(UIElement* e));
          } |]

instance Storable JoystickState where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(JoystickState) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<JoystickState>::AlignmentOf } |]
  peek ptr = do 
    _joystickStateJoystick <- toSDLJoystick <$> [C.exp| SDL_Joystick* {$(JoystickState* ptr)->joystick_} |]
    _joystickStateJoystickID <- fromIntegral <$> [C.exp| int {(int)$(JoystickState* ptr)->joystickID_} |]
    _joystickStateController <- toSDLController <$> [C.exp| SDL_GameController* {$(JoystickState* ptr)->controller_} |]
    _joystickStateScreenJoystick <- [C.exp| UIElement* {$(JoystickState* ptr)->screenJoystick_} |]
    _joystickStateName <- peekCString =<< [C.exp| const char* {$(JoystickState* ptr)->name_.CString()} |]
    _joystickStateButtons <- peekForeignVectorAs =<< [C.exp| PODVectorBool* {&$(JoystickState* ptr)->buttons_} |]
    _joystickStateButtonPress <- peekForeignVectorAs =<< [C.exp| PODVectorBool* {&$(JoystickState* ptr)->buttonPress_} |]
    _joystickStateAxes <- peekForeignVectorAs =<< [C.exp| PODVectorFloat* {&$(JoystickState* ptr)->axes_} |]
    _joystickStateHats <- peekForeignVectorAs =<< [C.exp| PODVectorInt* {&$(JoystickState* ptr)->hats_} |]
    return $ JoystickState {..}

  poke ptr JoystickState{..} = 
    withCString _joystickStateName $ \_joystickStateName' -> 
    withForeignVector () _joystickStateButtons $ \_joystickStateButtons' ->
    withForeignVector () _joystickStateButtonPress $ \_joystickStateButtonPress' ->
    withForeignVector () _joystickStateAxes $ \_joystickStateAxes' ->
    withForeignVector () _joystickStateHats $ \_joystickStateHats' ->
      [C.block| void { 
        $(JoystickState* ptr)->joystick_ = $(SDL_Joystick* _joystickStateJoystick');
        $(JoystickState* ptr)->joystickID_ = (SDL_JoystickID)$(int _joystickStateJoystickID');
        $(JoystickState* ptr)->controller_ = $(SDL_GameController* _joystickStateController');
        $(JoystickState* ptr)->screenJoystick_ = $(UIElement* _joystickStateScreenJoystick');
        $(JoystickState* ptr)->name_ = String($(const char* _joystickStateName'));
        $(JoystickState* ptr)->buttons_ = PODVectorBool(*$(PODVectorBool* _joystickStateButtons'));
        $(JoystickState* ptr)->buttonPress_ = PODVectorBool(*$(PODVectorBool* _joystickStateButtonPress'));
        $(JoystickState* ptr)->axes_ = PODVectorFloat(*$(PODVectorFloat* _joystickStateAxes'));
        $(JoystickState* ptr)->hats_ = PODVectorInt(*$(PODVectorInt* _joystickStateHats'));
      } |]
    where 
    _joystickStateJoystick' = fromSDLJoystick _joystickStateJoystick
    _joystickStateJoystickID' = fromIntegral _joystickStateJoystickID
    _joystickStateController' = fromSDLController _joystickStateController
    _joystickStateScreenJoystick' = _joystickStateScreenJoystick

mousePoistionOffscreen :: IntVector2 
mousePoistionOffscreen = unsafePerformIO $ peek =<< [C.exp| const IntVector2* {&MOUSE_POSITION_OFFSCREEN} |]

-- | Poll for window messages. Called by HandleBeginFrame().
inputUpdate :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m ()
inputUpdate p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Input* ptr)->Update() } |]

-- | Set whether ALT-ENTER fullscreen toggle is enabled.
inputSetToggleFullscreen :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Bool -- ^ enable
  -> m ()
inputSetToggleFullscreen p e = liftIO $ do 
  let ptr = parentPointer p
      e' = fromBool e 
  [C.exp| void { $(Input* ptr)->SetToggleFullscreen($(int e')) } |]

-- | Set whether the operating system mouse cursor is visible. When not visible (default), is kept centered to prevent leaving the window. Mouse visibility event can be suppressed-- this also recalls any unsuppressed SetMouseVisible which can be returned by ResetMouseVisible().
inputSetMouseVisible :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Bool -- ^ Flag of visibility for mouse
  -> Bool -- ^ supress event (def false)
  -> m () 
inputSetMouseVisible p flag se = liftIO $ do 
  let ptr = parentPointer p 
      flag' = fromBool flag
      se' = fromBool se
  [C.exp| void { $(Input* ptr)->SetMouseVisible($(int flag'), $(int se')) } |]

-- | Reset last mouse visibility that was not suppressed in SetMouseVisible.
inputResetMouseVisible :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m ()
inputResetMouseVisible p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Input* ptr)->ResetMouseVisible() } |]

-- | Set whether the mouse is currently being grabbed by an operation.
inputSetMouseGrabbed :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Bool -- ^ grab
  -> Bool -- ^ supress event (def false)
  -> m ()
inputSetMouseGrabbed p gr se = liftIO $ do 
  let ptr = parentPointer p 
      se' = fromBool se 
      gr' = fromBool gr 
  [C.exp| void { $(Input* ptr)->SetMouseGrabbed($(int gr'), $(int se')) } |]

-- | Reset the mouse grabbed to the last unsuppressed SetMouseGrabbed call
inputResetMouseGrabbed :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m ()
inputResetMouseGrabbed p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Input* ptr)->ResetMouseGrabbed() } |]

-- | Set the mouse mode.
--
-- Set the mouse mode behaviour.
--
-- MM_ABSOLUTE is the default behaviour, allowing the toggling of operating system cursor visibility and allowing the cursor to escape the window when visible.
-- When the operating system cursor is invisible in absolute mouse mode, the mouse is confined to the window.
-- If the operating system and UI cursors are both invisible, interaction with the Urho UI will be limited (eg: drag move / drag end events will not trigger).
-- SetMouseMode(MM_ABSOLUTE) will call SetMouseGrabbed(false).
-- 
-- MM_RELATIVE sets the operating system cursor to invisible and confines the cursor to the window.
-- The operating system cursor cannot be set to be visible in this mode via SetMouseVisible(), however changes are tracked and will be restored when another mouse mode is set.
-- When the virtual cursor is also invisible, UI interaction will still function as normal (eg: drag events will trigger).
-- SetMouseMode(MM_RELATIVE) will call SetMouseGrabbed(true).
-- 
-- MM_WRAP grabs the mouse from the operating system and confines the operating system cursor to the window, wrapping the cursor when it is near the edges.
-- SetMouseMode(MM_WRAP) will call SetMouseGrabbed(true).
-- 
-- MM_FREE does not grab/confine the mouse cursor even when it is hidden. This can be used for cases where the cursor should render using the operating system
-- outside the window, and perform custom rendering (with SetMouseVisible(false)) inside.
inputSetMouseMode :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> MouseMode -- ^ mode 
  -> Bool -- ^ supress event (default false)
  -> m ()
inputSetMouseMode p mm se = liftIO $ do 
  let ptr = parentPointer p 
      mm' = fromIntegral . fromEnum $ mm 
      se' = fromBool se
  [C.exp| void { $(Input* ptr)->SetMouseMode((MouseMode)$(int mm'), $(int se')) } |]

-- | Reset the last mouse mode that wasn't suppressed in SetMouseMode
inputResetMouseMode :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m ()
inputResetMouseMode p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Input* ptr)->ResetMouseMode() } |]

-- | Add screen joystick.
--
-- Return the joystick instance ID when successful or negative on error.
-- If layout file is not given, use the default screen joystick layout.
-- If style file is not given, use the default style file from root UI element.
--
-- This method should only be called in main thread.
inputAddScreenJoystick :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Ptr XMLFile -- ^ layout file, could be nullPtr
  -> Ptr XMLFile -- ^ style file, could be nullPtr
  -> m JoystickID -- ^ Joystick ID
inputAddScreenJoystick p layoutFile styleFile = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| SDL_JoystickID { $(Input* ptr)->AddScreenJoystick($(XMLFile* layoutFile), $(XMLFile* styleFile)) } |]

-- | Remove screen joystick by instance ID.
-- Return true if successful.
--
-- This method should only be called in main thread.
inputRemoveScreenJoystick :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> JoystickID -- ^ id 
  -> m ()
inputRemoveScreenJoystick p i = liftIO $ do 
  let ptr = parentPointer p 
      i' = fromIntegral i 
  [C.exp| void { $(Input* ptr)->RemoveScreenJoystick((SDL_JoystickID)$(int i')) } |]

-- | Set whether the virtual joystick is visible.
inputSetScreenJoystickVisible :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> JoystickID -- ^ id
  -> Bool -- ^ enable
  -> m ()
inputSetScreenJoystickVisible p i e = liftIO $ do 
  let ptr = parentPointer p 
      i' = fromIntegral i
      e' = fromBool e
  [C.exp| void { $(Input* ptr)->SetScreenJoystickVisible((SDL_JoystickID)$(int i'), $(int e') != 0) } |]

-- | Show or hide on-screen keyboard on platforms that support it. When shown, keypresses from it are delivered as key events.
inputSetScreenKeyboardVisible :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Bool -- ^ enable
  -> m ()
inputSetScreenKeyboardVisible p e = liftIO $ do 
  let ptr = parentPointer p 
      e' = fromBool e
  [C.exp| void { $(Input* ptr)->SetScreenKeyboardVisible($(int e')) } |]

-- | Set touch emulation by mouse. Only available on desktop platforms. When enabled, actual mouse events are no longer sent and the mouse cursor is forced visible.
inputSetTouchEmulation :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Bool -- ^ enable
  -> m ()
inputSetTouchEmulation p e = liftIO $ do 
  let ptr = parentPointer p 
      e' = fromBool e
  [C.exp| void { $(Input* ptr)->SetTouchEmulation($(int e')) } |]

-- | Begin recording a touch gesture. Return true if successful. The E_GESTURERECORDED event (which contains the ID for the new gesture) will be sent when recording finishes.
inputRecordGesture :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputRecordGesture p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->RecordGesture() } |]

-- | Save all in-memory touch gestures. Return true if successful.
inputSaveGestures :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Ptr Serializer -- ^ dest
  -> m  Bool
inputSaveGestures p s = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->SaveGestures(*$(Serializer* s)) } |]

-- | Save a specific in-memory touch gesture to a file. Return true if successful.
inputSaveGesture :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Ptr Serializer -- ^ dest
  -> Word -- ^ gesture id
  -> m Bool
inputSaveGesture p s gid = liftIO $ do 
  let ptr = parentPointer p 
      gid' = fromIntegral gid
  toBool <$> [C.exp| int { (int) $(Input* ptr)->SaveGesture(*$(Serializer* s), $(unsigned int gid')) } |]

-- | Load touch gestures from a file. Return number of loaded gestures, or 0 on failure.
inputLoadGestures :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Ptr Deserializer -- ^ source
  -> m Word
inputLoadGestures p d = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| unsigned int { $(Input* ptr)->LoadGestures(*$(Deserializer* d)) } |]

-- | Remove an in-memory gesture by ID. Return true if was found.
inputRemoveGesture :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Word -- ^ gesture id
  -> m Bool
inputRemoveGesture p gid = liftIO $ do 
  let ptr = parentPointer p 
      gid' = fromIntegral gid
  toBool <$> [C.exp| int { (int) $(Input* ptr)->RemoveGesture($(unsigned int gid')) } |]

-- | Remove all in-memory gestures.
inputRemoveAllGestures :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m ()
inputRemoveAllGestures p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Input* ptr)->RemoveAllGestures() } |]

-- | Return keycode from key name.
inputGetKeyFromName :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> String -- ^ name
  -> m Key
inputGetKeyFromName p n = liftIO $ withCString n $ \n' -> do 
  let ptr = parentPointer p 
  fromUrhoKey . fromIntegral <$> [C.exp| int { $(Input* ptr)->GetKeyFromName(String($(const char* n'))) } |]

-- | Return keycode from scancode.
inputGetKeyFromScancode :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Int -- ^ scancode
  -> m Key
inputGetKeyFromScancode p sc = liftIO $ do 
  let ptr = parentPointer p 
      sc' = fromIntegral sc
  fromUrhoKey . fromIntegral <$> [C.exp| int { $(Input* ptr)->GetKeyFromScancode($(int sc')) } |]

-- | Return name of key from keycode.
inputGetKeyName :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Key -- ^ key
  -> m String
inputGetKeyName p k = liftIO $ do 
  let ptr = parentPointer p 
      k' = fromIntegral . toUrhoKey $ k
  peekCString =<< [C.exp| const char* { $(Input* ptr)->GetKeyName($(int k')).CString() } |]

-- | Return scancode from keycode.
inputGetScancodeFromKey :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Key -- ^ key
  -> m Int
inputGetScancodeFromKey p k = liftIO $ do 
  let ptr = parentPointer p 
      k' = fromIntegral . toUrhoKey $ k
  fromIntegral <$> [C.exp| int { $(Input* ptr)->GetScancodeFromKey($(int k')) } |]

-- | Return scancode from key name.
inputGetScancodeFromName :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> String -- ^ name
  -> m Int
inputGetScancodeFromName p n = liftIO $ withCString n $ \n' -> do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int { $(Input* ptr)->GetScancodeFromName(String($(const char* n'))) } |]

-- | Return name of key from scancode.
inputGetScancodeName :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Int -- ^ scancode
  -> m String
inputGetScancodeName p sc = liftIO $ do 
  let ptr = parentPointer p
      sc' = fromIntegral sc 
  peekCString =<< [C.exp| const char* { $(Input* ptr)->GetScancodeName($(int sc')).CString() } |]

-- | Check if a key is held down.
inputGetKeyDown :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Key -- ^ Key to test
  -> m Bool
inputGetKeyDown p k = liftIO $ do 
  let ptr = parentPointer p 
      ki = fromIntegral . toUrhoKey $ k
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetKeyDown($(int ki)) } |]

-- | Check if a key has been pressed on this frame.
inputGetKeyPress :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Key -- ^ key
  -> m Bool
inputGetKeyPress p k = liftIO $ do 
  let ptr = parentPointer p 
      k' = fromIntegral . toUrhoKey $ k 
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetKeyPress($(int k')) } |]

-- | Check if a key is held down by scancode.
inputGetScancodeDown :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Int -- ^ scancode
  -> m Bool
inputGetScancodeDown p sc = liftIO $ do 
  let ptr = parentPointer p
      sc' = fromIntegral sc  
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetScancodeDown($(int sc')) } |]

-- | Check if a key has been pressed on this frame by scancode.
inputGetScancodePress :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Int -- ^ scancode
  -> m Bool
inputGetScancodePress p sc = liftIO $ do 
  let ptr = parentPointer p
      sc' = fromIntegral sc
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetScancodePress($(int sc')) } |]

-- |  Check if a mouse button is held down.
inputGetMouseButtonDown :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Int -- ^ button
  -> m Bool
inputGetMouseButtonDown p b = liftIO $ do 
  let ptr = parentPointer p 
      b' = fromIntegral b 
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetMouseButtonDown($(int b')) } |]

-- | Check if a mouse button has been pressed on this frame.
inputGetMouseButtonPress :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Int -- ^ button
  -> m Bool
inputGetMouseButtonPress p b = liftIO $ do 
  let ptr = parentPointer p 
      b' = fromIntegral b
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetMouseButtonPress($(int b')) } |]

-- | Check if a qualifier key is held down.
inputGetQualifierDown :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Int -- ^ qualifier
  -> m Bool
inputGetQualifierDown p q = liftIO $ do 
  let ptr = parentPointer p 
      q' = fromIntegral q 
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetQualifierDown($(int q')) } |]

-- | Check if a qualifier key has been pressed on this frame.
inputGetQualifierPress :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Int -- ^ qualifier
  -> m Bool
inputGetQualifierPress p q = liftIO $ do 
  let ptr = parentPointer p 
      q' = fromIntegral q 
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetQualifierPress($(int q')) } |]

-- | Return the currently held down qualifiers.
inputGetQualifiers :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Int
inputGetQualifiers p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int { $(Input* ptr)->GetQualifiers() } |]

-- | Return mouse position within window. Should only be used with a visible mouse cursor.
inputGetMousePosition :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m IntVector2
inputGetMousePosition p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.block| IntVector2* { 
    static IntVector2 v = $(Input* ptr)->GetMousePosition();
    return &v; 
    } |]

-- | Return mouse movement since last frame.
inputGetMouseMove :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m IntVector2
inputGetMouseMove p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const IntVector2* { &$(Input* ptr)->GetMouseMove() } |]

-- | Return horizontal mouse movement since last frame.
inputGetMouseMoveX :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Int
inputGetMouseMoveX p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int { $(Input* ptr)->GetMouseMoveX() } |]

-- | Return vertical mouse movement since last frame.
inputGetMouseMoveY :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Int
inputGetMouseMoveY p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int { $(Input* ptr)->GetMouseMoveY() } |]

-- | Return mouse wheel movement since last frame.
inputGetMouseMoveWheel :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Int
inputGetMouseMoveWheel p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int { $(Input* ptr)->GetMouseMoveWheel() } |]

-- | Return number of active finger touches.
inputGetNumTouches :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Word
inputGetNumTouches p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| unsigned int {$(Input* ptr)->GetNumTouches()}|]

-- | Return active finger touch by index.
inputGetTouch :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Word
  -> m (Maybe TouchState)
inputGetTouch p i = liftIO $ do 
  let ptr = parentPointer p 
      i' = fromIntegral i 
  ts <- [C.exp| TouchState* {$(Input* ptr)->GetTouch($(unsigned int i'))} |]
  checkNullPtr' ts peek

-- | Return number of connected joysticks.
inputGetNumJoysticks :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Int
inputGetNumJoysticks p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int { $(Input* ptr)->GetNumJoysticks() } |]

-- | Return joystick state by ID, or null if does not exist.
inputGetJoystick :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> JoystickID -- ^ id
  -> m (Maybe (Ptr JoystickState))
inputGetJoystick p i = liftIO $ do 
  let ptr = parentPointer p 
      i' = fromIntegral i
  sp <- [C.exp| JoystickState* { $(Input* ptr)->GetJoystick($(SDL_JoystickID i')) } |]
  checkNullPtr' sp return

-- | Return joystick state by index, or null if does not exist. 0 = first connected joystick.
inputGetJoystickByIndex :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> Word -- ^ index
  -> m (Maybe (Ptr JoystickState))
inputGetJoystickByIndex p i = liftIO $ do 
  let ptr = parentPointer p 
      i' = fromIntegral i
  sp <- [C.exp| JoystickState* { $(Input* ptr)->GetJoystickByIndex($(unsigned int i')) } |]
  checkNullPtr' sp return

-- | Return joystick state by name, or null if does not exist.
inputGetJoystickByName :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> String -- ^ name
  -> m (Maybe (Ptr JoystickState))
inputGetJoystickByName p n = liftIO $ withCString n $ \n' -> do 
  let ptr = parentPointer p 
  sp <- [C.exp| JoystickState* { $(Input* ptr)->GetJoystickByName(String($(const char* n'))) } |]
  checkNullPtr' sp return

-- | Return whether fullscreen toggle is enabled.
inputGetToggleFullscreen :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputGetToggleFullscreen p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetToggleFullscreen() } |]

-- | Return whether a virtual joystick is visible.
inputIsScreenJoystickVisible :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> JoystickID -- ^ id
  -> m Bool
inputIsScreenJoystickVisible p i = liftIO $ do 
  let ptr = parentPointer p 
      i' = fromIntegral i
  toBool <$> [C.exp| int { (int) $(Input* ptr)->IsScreenJoystickVisible((SDL_JoystickID)$(int i')) } |]

-- | Return whether on-screen keyboard is supported.
inputGetScreenKeyboardSupport :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputGetScreenKeyboardSupport p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->GetScreenKeyboardSupport() } |]

-- | Return whether on-screen keyboard is being shown.
inputIsScreenKeyboardVisible :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputIsScreenKeyboardVisible p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->IsScreenKeyboardVisible() } |]

-- | Return whether touch emulation is enabled.
inputGetTouchEmulation :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputGetTouchEmulation p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->GetTouchEmulation() } |]

-- | Return whether the operating system mouse cursor is visible.
inputIsMouseVisible :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputIsMouseVisible p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->IsMouseVisible() } |]

-- | Return whether the mouse is currently being grabbed by an operation.
inputIsMouseGrabbed :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputIsMouseGrabbed p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->IsMouseGrabbed() } |]

-- | Return whether the mouse is locked to the window
inputIsMouseLocked :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputIsMouseLocked p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->IsMouseLocked() } |]

-- | Return the mouse mode.
inputGetMouseMode :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m MouseMode
inputGetMouseMode p = liftIO $ do 
  let ptr = parentPointer p 
  toEnum . fromIntegral <$> [C.exp| int { (int)$(Input* ptr)->GetMouseMode() } |]

-- | Return whether application window has input focus.
inputHasFocus :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputHasFocus p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->HasFocus() } |]

-- | Return whether application window is minimized.
inputIsMinimized :: (Parent Input a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Input or ascentor
  -> m Bool
inputIsMinimized p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int) $(Input* ptr)->IsMinimized() } |]
