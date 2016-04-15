{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.UI(
    UI
  , uiContext
  , uiRoot
  , uiFocusElement
  , uiCursor
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.UI
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.Cursor
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> uiCntx <> objectContext <> uiElementContext <> cursorContext)
C.include "<Urho3D/UI/UI.h>"
C.using "namespace Urho3D"

uiContext :: C.Context 
uiContext = objectContext <> uiCntx

deriveParent ''Object ''UI

instance Subsystem UI where 
  getSubsystemImpl ptr = [C.exp| UI* { $(Object* ptr)->GetSubsystem<UI>() } |]

-- | Returns root UI element
uiRoot :: (Pointer p a, Parent UI a, MonadIO m) => p -> m (Ptr UIElement)
uiRoot ptr = liftIO $ do 
  let ptr' = parentPointer ptr 
  [C.exp| UIElement* { $(UI* ptr')->GetRoot() } |]

-- | Returns current element in focus
uiFocusElement :: (Pointer p a, Parent UI a, MonadIO m) => p -> m (Maybe (Ptr UIElement))
uiFocusElement ptr = liftIO $ do 
  let ptr' = parentPointer ptr 
  fe <- [C.exp| UIElement* { $(UI* ptr')->GetFocusElement() } |]
  checkNullPtr' fe return

-- | Returns ui cursor
uiCursor :: (Pointer p a, Parent UI a, MonadIO m) => p -> m (Ptr Cursor)
uiCursor p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Cursor* { $(UI* ptr)->GetCursor() } |]