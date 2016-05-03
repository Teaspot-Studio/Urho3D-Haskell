{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Input.Input(
    Input
  , MouseMode(..)
  , JoystickID
  -- | Touch State
  , TouchState
  , touchedElement 
  , touchID 
  , touchPosition 
  , touchLastPosition 
  , touchDelta 
  , touchPressure
  -- | Input API
  , inputContext
  , mousePoistionOffscreen
  , getNumJoysticks
  , addScreenJoystick
  , setScreenJoystickVisible
  , inputGetNumTouches
  , inputGetTouch
  , inputSetMouseVisible
  , inputGetMouseMove
  , inputGetKeyDown
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Input.Internal.Input
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Resource.XMLFile
import Graphics.Urho3D.Math.Vector2 
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Data.Monoid
import Foreign 
import Foreign.C.String
import Text.RawString.QQ
import Control.Lens 
import Data.Char 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> inputCntx <> objectContext <> xmlFileContext <> vector2Context <> uiElementContext <> vectorContext)
C.include "<Urho3D/Input/Input.h>"
C.using "namespace Urho3D"

C.verbatim "typedef WeakPtr<UIElement> SharedWeakUIElement;"
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
    e <- wrapSharedWeakUIElementPtr =<< [C.exp| SharedWeakUIElement* { new WeakPtr<UIElement>($(TouchState* ptr)->touchedElement_) }|]
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

-- | Returns number of known joysticks
getNumJoysticks :: MonadIO m => Ptr Input -> m Int 
getNumJoysticks ptr = liftIO $ fromIntegral <$> [C.exp| int {$(Input* ptr)->GetNumJoysticks()} |]

-- | Add screen joystick
-- Return the joystick instance ID when successful or negative on error.
-- If layout file is not given, use the default screen joystick layout.
-- If style file is not given, use the default style file from root UI element.
--
-- This method should only be called in main thread.
addScreenJoystick :: MonadIO m => Ptr Input 
  -> Ptr XMLFile -- ^ layout file, could be nullPtr
  -> Ptr XMLFile -- ^ style file, could be nullPtr
  -> m JoystickID -- ^ Joystick ID
addScreenJoystick ptr layoutFile styleFile = liftIO $ do 
  [C.exp| SDL_JoystickID { $(Input* ptr)->AddScreenJoystick($(XMLFile* layoutFile), $(XMLFile* styleFile)) } |]

-- | Set whether the virtual joystick is visible.
setScreenJoystickVisible :: MonadIO m => Ptr Input 
  -> JoystickID 
  -> Bool -- ^ Visibility flag
  -> m ()
setScreenJoystickVisible ptr jid flag = liftIO $
  [C.exp| void { $(Input* ptr)->SetScreenJoystickVisible($(SDL_JoystickID jid), $(int flag') != 0) } |]
  where flag' = if flag then 1 else 0

-- | Return number of active finger touches.
inputGetNumTouches :: (Parent Input a, Pointer p a, MonadIO m) => p 
  -> m Int 
inputGetNumTouches p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int {$(Input* ptr)->GetNumTouches()}|]

-- | Return active finger touch by index.
inputGetTouch :: (Parent Input a, Pointer p a, MonadIO m) => p 
  -> Int -- ^ Index of touch
  -> m (Maybe TouchState)
inputGetTouch p i = liftIO $ do 
  let ptr = parentPointer p 
      i' = fromIntegral i 
  ts <- [C.exp| TouchState* {$(Input* ptr)->GetTouch($(int i'))} |]
  checkNullPtr' ts peek

-- | Setting visibility of mouse
inputSetMouseVisible :: (Parent Input a, Pointer p a, MonadIO m) => p 
  -> Bool -- ^ Flag of visibility for mouse
  -> m () 
inputSetMouseVisible p flag = liftIO $ do 
  let ptr = parentPointer p 
      flag' = fromBool flag
  [C.exp| void { $(Input* ptr)->SetMouseVisible($(int flag')) } |]

-- | Return mouse movement since last frame.
inputGetMouseMove :: (Parent Input a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to input
  -> m IntVector2
inputGetMouseMove p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const IntVector2* { &$(Input* ptr)->GetMouseMove() } |]

-- | Check if a key is held down.
inputGetKeyDown :: (Parent Input a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to input
  -> Char -- ^ Key to test
  -> m Bool
inputGetKeyDown p k = liftIO $ do 
  let ptr = parentPointer p 
      ki = fromIntegral . ord $ k
  toBool <$> [C.exp| int { (int)$(Input* ptr)->GetKeyDown($(int ki)) } |]
  