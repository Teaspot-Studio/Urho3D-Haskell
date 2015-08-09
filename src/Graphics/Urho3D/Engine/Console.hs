{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Engine.Console(
    Console
  , consoleContext
  , consoleSetDefaultStyle
  , consoleGetBackground
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Engine.Internal.Console
import Graphics.Urho3D.Resource.XMLFile 
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> consoleCntx <> contextContext <> objectContext <> xmlFileContext <> borderImageContext)
C.include "<Urho3D/Engine/Console.h>"
C.using "namespace Urho3D"

consoleContext :: C.Context 
consoleContext = consoleCntx <> objectContext

newConsole :: Ptr Context -> IO (Ptr Console)
newConsole ptr = [C.exp| Console* {new Console($(Context* ptr))} |]

deleteConsole :: Ptr Console -> IO ()
deleteConsole ptr = [C.exp| void {delete $(Console* ptr)} |]

instance Createable (Ptr Console) where 
  type CreationOptions (Ptr Console) = Ptr Context 

  newObject = liftIO . newConsole
  deleteObject = liftIO . deleteConsole

instance Parent Object Console where 
  castToParent ptr = [C.pure| Object* {(Object*)$(Console* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Console* {(Console*)$(Object* ptr)} |]
    in if child == nullPtr then Nothing else Just child

-- | Set UI elements style from an XML file
consoleSetDefaultStyle :: (Pointer p Console, MonadIO m) => p -- ^ Console ptr 
  -> Ptr XMLFile -- ^ style file
  -> m ()
consoleSetDefaultStyle p file = liftIO $ do 
  let ptr = pointer p 
  [C.exp| void { $(Console* ptr)->SetDefaultStyle($(XMLFile* file)) }|]

-- | Return the background element.
consoleGetBackground :: (Pointer p Console, MonadIO m) => p -- ^ Console ptr 
  -> m (Ptr BorderImage)
consoleGetBackground p = liftIO $ do 
  let ptr = pointer p 
  [C.exp| BorderImage* { $(Console* ptr)->GetBackground() }|]