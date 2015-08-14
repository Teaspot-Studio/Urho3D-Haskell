{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.C.String 

C.context (C.cppCtx <> fileSystemCntx <> objectContext)
C.include "<Urho3D/IO/FileSystem.h>"
C.include "<SDL/SDL.h>"
C.include "<SDL/SDL_stdinc.h>"
C.include "<iostream>"
C.using "namespace Urho3D"

fileSystemContext :: C.Context 
fileSystemContext = objectContext <> fileSystemCntx

instance Parent Object FileSystem where 
  castToParent ptr = [C.pure| Object* { (Object*)$(FileSystem* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| FileSystem* { (FileSystem*)$(Object* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Subsystem FileSystem where 
  getSubsystemImpl ptr = [C.exp| FileSystem* { $(Object* ptr)->GetSubsystem<FileSystem>() } |]

-- | Returns application preferences directory
getAppPreferencesDir :: (Parent FileSystem a, Pointer p a, MonadIO m) => p -> String -> String -> m String 
getAppPreferencesDir p org app = liftIO $ withCString org $ \org' -> withCString app $ \app' -> do 
  res <- [C.exp| const char* { 
    SDL_GetPrefPath(String($(const char* org')).CString(), String($(const char* app')).CString())
    } |]
  str <- peekCString res
  [C.exp| void { SDL_free((void*)$(char* res)) } |]
  putStrLn =<< getProgramDir p
  return $ internalPath <$> str
  where 
    internalPath '\\' = '/'
    internalPath c = c 

-- | Returns application executable directory
getProgramDir :: (Parent FileSystem a, Pointer p a, MonadIO m) => p -> m String 
getProgramDir p = liftIO $ do 
  let ptr = parentPointer p
  res <- [C.exp| const char* { $(FileSystem* ptr)->GetProgramDir().CString() } |]
  peekCString res