{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Engine.Engine(
    Engine
  , engineContext
  , SharedEngine 
  , SharedEnginePtr
  , engineDumpResources
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Engine.Internal.Engine
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> engineCntx <> sharedEnginePtrCntx <> contextContext <> objectContext)
C.include "<Urho3D/Engine/Engine.h>"
C.using "namespace Urho3D"

engineContext :: C.Context 
engineContext = engineCntx <> sharedEnginePtrCntx

newEngine :: Ptr Context -> IO (Ptr Engine)
newEngine ptr = [C.exp| Engine* {new Engine($(Context* ptr))} |]

deleteEngine :: Ptr Engine -> IO ()
deleteEngine ptr = [C.exp| void {delete $(Engine* ptr)} |]

instance Createable Engine where 
  type CreationOptions Engine = Ptr Context 

  newObject = liftIO . newEngine
  deleteObject = liftIO . deleteEngine

instance Parent Object Engine where 
  castToParent ptr = [C.pure| Object* {(Object*)$(Engine* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Engine* {(Engine*)$(Object* ptr)} |]
    in if child == nullPtr then Nothing else Just child

sharedPtr "Engine"

-- | Prints all resources to log
engineDumpResources :: (MonadIO m, Pointer r Engine) => r -- ^ Pointer to engine
  -> Bool -- ^ Print also filenames?
  -> m ()
engineDumpResources ref dumpFilenames = liftIO $ [C.exp| void {$(Engine* p)->DumpResources($(int v) != 0)} |]
  where 
  p = pointer ref
  v = if dumpFilenames then 1 else 0