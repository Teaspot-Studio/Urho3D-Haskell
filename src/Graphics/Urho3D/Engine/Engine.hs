{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Engine.Engine(
    Engine
  , engineContext
  , SharedEngine 
  , engineDumpResources
  , engineCreateConsole
  , engineCreateDebugHud
  , engineExit
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
import Graphics.Urho3D.Parent
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

deriveParent ''Object ''Engine

sharedPtr "Engine" 

-- | Prints all resources to log
engineDumpResources :: (Pointer p a, Parent Engine a, MonadIO m) => p -- ^ Pointer to engine
  -> Bool -- ^ Print also filenames?
  -> m ()
engineDumpResources p dumpFilenames = liftIO $ [C.exp| void {$(Engine* ptr)->DumpResources($(int v) != 0)} |]
  where 
  ptr = parentPointer p
  v = if dumpFilenames then 1 else 0

-- | Create the console and return it. May return Nothing if engine configuration does not allow creation (headless mode.)
engineCreateConsole :: (Pointer p a, Parent Engine a, MonadIO m) => p -- ^ Pointer to engine
  -> m (Maybe (Ptr Console))
engineCreateConsole p = liftIO $ do 
  let ptr = parentPointer p 
  cp <- [C.exp| Console* { $(Engine* ptr)->CreateConsole() } |]
  checkNullPtr' cp return

-- | Create the console and return it. May return Nothing if engine configuration does not allow creation (headless mode.)
engineCreateDebugHud :: (Pointer p a, Parent Engine a, MonadIO m) => p -- ^ Pointer to engine
  -> m (Ptr DebugHud)
engineCreateDebugHud p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| DebugHud* { $(Engine* ptr)->CreateDebugHud() } |]

-- | Issues command to engine to exit from application
engineExit :: (Pointer p a, Parent Engine a, MonadIO m) => p -- ^ Pointer to engine (or child)
  -> m ()
engineExit p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void{ $(Engine* ptr)->Exit() }|]