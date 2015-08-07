{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.Object(
    Object
  , objectContext
  , getSubsystem
  , Subsystem(..)
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.Object
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> objectCntx <> contextCntx)
C.include "<Urho3D/Core/Object.h>"
C.using "namespace Urho3D"

objectContext :: C.Context 
objectContext = objectCntx

-- | A subsystem of Urho3D, that can be aquired by Object API
class Subsystem a where 
  getSubsystemImpl :: Ptr Object -> IO (Ptr a)

-- | Returns specified subsystem from Object Context
getSubsystem :: (Subsystem a, Parent Object b) => Ptr b -> IO (Maybe (Ptr a))
getSubsystem ptr = do
  res <- getSubsystemImpl (castToParent ptr)
  checkNullPtr' res return