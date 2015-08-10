{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Input.Input(
    Input
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
  , getNumJoysticks
  , addScreenJoystick
  , setScreenJoystickVisible
  , inputGetNumTouches
  , inputGetTouch
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Input.Internal.Input
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Resource.XMLFile
import Graphics.Urho3D.Math.Vector2 
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Text.RawString.QQ
import Control.Lens 
import Data.Maybe

C.context (C.cppCtx <> inputCntx <> objectContext <> xmlFileContext <> vector2Context <> uiElementContext)
C.include "<Urho3D/Input/Input.h>"
C.using "namespace Urho3D"

inputContext :: C.Context 
inputContext = objectContext <> inputCntx <> vector2Context

instance Parent Object Input where 
  castToParent ptr = [C.pure| Object* { (Object*)$(Input* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Input* { (Input*)$(Object* ptr) } |]
    in if child == nullPtr then Nothing else Just child

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
    me <- [C.exp| UIElement* { $(TouchState* ptr)->touchedElement_.Get() }|]
    e <- checkNullPtr' me return
    return $ TouchState e tid pv lpv dv pr 

  poke ptr ts = do 
    with (ts ^. touchPosition) $ \pv -> 
      with (ts ^. touchLastPosition) $ \lpv -> 
        with (ts ^. touchDelta) $ \dv -> do
          let tid = fromIntegral $ ts ^. touchID 
              pr = realToFrac $ ts ^. touchPressure
              e = fromMaybe nullPtr $ ts ^. touchedElement
          [C.block| void { 
            $(TouchState* ptr)->touchID_ = $(float tid);
            $(TouchState* ptr)->position_ = *$(IntVector2* pv);
            $(TouchState* ptr)->lastPosition_ = *$(IntVector2* lpv);
            $(TouchState* ptr)->delta_ = *$(IntVector2* dv);
            $(TouchState* ptr)->pressure_ = $(float pr);
            $(TouchState* ptr)->touchedElement_ = WeakPtr<UIElement>($(UIElement* e));
          } |]


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