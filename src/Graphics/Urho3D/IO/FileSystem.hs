{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.IO.FileSystem(
    FileSystem
  , fileSystemContext
  , getAppPreferencesDir
  , getProgramDir
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.IO.Internal.FileSystem
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Data.Monoid
import Foreign.C.String 

C.context (C.cppCtx <> fileSystemCntx <> objectContext <> stringContext)
C.include "<Urho3D/IO/FileSystem.h>"
C.using "namespace Urho3D"

fileSystemContext :: C.Context 
fileSystemContext = objectContext <> fileSystemCntx

deriveParent ''Object ''FileSystem

instance Subsystem FileSystem where 
  getSubsystemImpl ptr = [C.exp| FileSystem* { $(Object* ptr)->GetSubsystem<FileSystem>() } |]


-- | Returns application preferences directory
getAppPreferencesDir :: (Parent FileSystem a, Pointer p a, MonadIO m) => p -> String -> String -> m String 
getAppPreferencesDir p org app = liftIO $ withCString org $ \org' -> withCString app $ \app' -> do
  let ptr = parentPointer p
  loadUrhoString =<< [C.exp| String* {
      new String( $(FileSystem* ptr)->GetAppPreferencesDir(String($(const char* org')), String($(const char* app'))) )
    } |]

-- | Returns application executable directory
getProgramDir :: (Parent FileSystem a, Pointer p a, MonadIO m) => p -> m String 
getProgramDir p = liftIO $ do 
  let ptr = parentPointer p
  loadUrhoString =<< [C.exp| String* { new String($(FileSystem* ptr)->GetProgramDir()) } |]