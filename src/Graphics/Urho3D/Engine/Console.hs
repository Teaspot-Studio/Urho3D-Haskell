{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Engine.Console(
    Console
  , consoleContext
  , consoleSetDefaultStyle
  , consoleGetBackground
  , consoleIsVisible
  , consoleSetVisible
  , consoleToggle
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

instance Subsystem Console where 
  getSubsystemImpl ptr = [C.exp| Console* { $(Object* ptr)->GetSubsystem<Console>() } |]

-- | Set UI elements style from an XML file
consoleSetDefaultStyle :: (Pointer p a, Parent Console a, MonadIO m) => p -- ^ Console ptr 
  -> Ptr XMLFile -- ^ style file
  -> m ()
consoleSetDefaultStyle p file = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Console* ptr)->SetDefaultStyle($(XMLFile* file)) }|]

-- | Return the background element.
consoleGetBackground :: (Pointer p a, Parent Console a, MonadIO m) => p -- ^ Console ptr 
  -> m (Ptr BorderImage)
consoleGetBackground p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| BorderImage* { $(Console* ptr)->GetBackground() }|]

-- | Returns True if console is visible to user
consoleIsVisible :: (Pointer p a, Parent Console a, MonadIO m) => p -- ^ Console ptr (or child)
  -> m Bool
consoleIsVisible p = liftIO $ do 
  let ptr = parentPointer p 
  (/= 0) <$> [C.exp| int { (int)$(Console* ptr)->IsVisible()} |]

-- | Returns True if console is visible to user
consoleSetVisible :: (Pointer p a, Parent Console a, MonadIO m) => p -- ^ Console ptr (or child)
  -> Bool -- ^ Flag, True - visible, False - hided
  -> m ()
consoleSetVisible p flag = liftIO $ do 
  let ptr = parentPointer p
      flag' = if flag then 1 else 0 
  [C.exp| void { $(Console* ptr)->SetVisible($(int flag') != 0)} |]

-- | Toggles visibility
consoleToggle :: (Pointer p a, Parent Console a, MonadIO m) => p -- ^ Console ptr (or child)
  -> m ()
consoleToggle p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Console* ptr)->Toggle() } |]