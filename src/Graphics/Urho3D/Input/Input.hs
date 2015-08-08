{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Input.Input(
    Input
  , inputContext
  , getNumJoysticks
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Input.Internal.Input
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> inputCntx <> objectContext)
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
getNumJoysticks :: Ptr Input -> IO Int 
getNumJoysticks ptr = fromIntegral <$> [C.exp| int {$(Input* ptr)->GetNumJoysticks()} |]