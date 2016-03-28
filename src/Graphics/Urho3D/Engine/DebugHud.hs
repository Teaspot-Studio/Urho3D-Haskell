{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Engine.DebugHud(
    DebugHud
  , debugHudContext
  , debugHudSetDefaultStyle
  , debugHudToggle
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Engine.Internal.DebugHud
import Graphics.Urho3D.Resource.XMLFile 
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> debugHudCntx <> contextContext <> objectContext <> xmlFileContext)
C.include "<Urho3D/Engine/DebugHud.h>"
C.using "namespace Urho3D"

debugHudContext :: C.Context 
debugHudContext = debugHudCntx <> objectContext

newDebugHud :: Ptr Context -> IO (Ptr DebugHud)
newDebugHud ptr = [C.exp| DebugHud* {new DebugHud($(Context* ptr))} |]

deleteDebugHud :: Ptr DebugHud -> IO ()
deleteDebugHud ptr = [C.exp| void {delete $(DebugHud* ptr)} |]

instance Createable (Ptr DebugHud) where 
  type CreationOptions (Ptr DebugHud) = Ptr Context 

  newObject = liftIO . newDebugHud
  deleteObject = liftIO . deleteDebugHud

instance Parent Object DebugHud where 
  castToParent ptr = [C.pure| Object* {(Object*)$(DebugHud* ptr)} |]
  castToChild ptr = let
    child = [C.pure| DebugHud* {(DebugHud*)$(Object* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Subsystem DebugHud where 
  getSubsystemImpl ptr = [C.exp| DebugHud* { $(Object* ptr)->GetSubsystem<DebugHud>() } |]

-- | Set UI elements style from an XML file
debugHudSetDefaultStyle :: (Pointer p a, Parent DebugHud a, MonadIO m) => p -- ^ DebugHud ptr 
  -> Ptr XMLFile -- ^ style file
  -> m ()
debugHudSetDefaultStyle p file = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(DebugHud* ptr)->SetDefaultStyle($(XMLFile* file)) }|]

-- | Toggles visibility of debug HUD
debugHudToggle :: (Pointer p a, Parent DebugHud a, MonadIO m) => p -- ^ DebugHud ptr or child
  -> m ()
debugHudToggle p = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| void { $(DebugHud* ptr)->ToggleAll() } |]