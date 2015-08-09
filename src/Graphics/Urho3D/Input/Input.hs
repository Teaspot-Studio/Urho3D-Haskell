{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Input.Input(
    Input
  , inputContext
  , getNumJoysticks
  , addScreenJoystick
  , setScreenJoystickVisible
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Input.Internal.Input
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Resource.XMLFile
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> inputCntx <> objectContext <> xmlFileContext)
C.include "<Urho3D/Input/Input.h>"
C.using "namespace Urho3D"

inputContext :: C.Context 
inputContext = objectContext <> inputCntx

instance Parent Object Input where 
  castToParent ptr = [C.pure| Object* { (Object*)$(Input* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Input* { (Input*)$(Object* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Subsystem Input where 
  getSubsystemImpl ptr = [C.exp| Input* { $(Object* ptr)->GetSubsystem<Input>() } |]

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