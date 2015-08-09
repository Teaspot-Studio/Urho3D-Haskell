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
  , SharedEnginePtr(..)
  , wrapSharedEnginePtr
  , engineDumpResources
  , engineCreateConsole
  , engineCreateDebugHud
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Engine.Internal.Engine
import Graphics.Urho3D.Engine.Console 
import Graphics.Urho3D.Engine.DebugHud
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> engineCntx <> sharedEnginePtrCntx <> contextContext <> objectContext <> consoleContext <> debugHudContext)
C.include "<Urho3D/Engine/Engine.h>"
C.using "namespace Urho3D"

engineContext :: C.Context 
engineContext = engineCntx <> sharedEnginePtrCntx

newEngine :: Ptr Context -> IO (Ptr Engine)
newEngine ptr = [C.exp| Engine* {new Engine($(Context* ptr))} |]

deleteEngine :: Ptr Engine -> IO ()
deleteEngine ptr = [C.exp| void {delete $(Engine* ptr)} |]

instance Createable (Ptr Engine) where 
  type CreationOptions (Ptr Engine) = Ptr Context 

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

-- | Create the console and return it. May return Nothing if engine configuration does not allow creation (headless mode.)
engineCreateConsole :: (MonadIO m, Pointer p Engine) => p -- ^ Pointer to engine
  -> m (Maybe (Ptr Console))
engineCreateConsole p = liftIO $ do 
  let ptr = pointer p 
  cp <- [C.exp| Console* { $(Engine* ptr)->CreateConsole() } |]
  checkNullPtr' cp return

-- | Create the console and return it. May return Nothing if engine configuration does not allow creation (headless mode.)
engineCreateDebugHud :: (MonadIO m, Pointer p Engine) => p -- ^ Pointer to engine
  -> m (Ptr DebugHud)
engineCreateDebugHud p = liftIO $ do 
  let ptr = pointer p 
  [C.exp| DebugHud* { $(Engine* ptr)->CreateDebugHud() } |]