{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.ToolTip(
    ToolTip 
  , toolTipContext
  , SharedToolTip
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.ToolTip
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Parent 

C.context (C.cppCtx <> sharedToolTipPtrCntx <> toolTipCntx <> contextContext <> uiElementContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/UI/ToolTip.h>"
C.using "namespace Urho3D"

toolTipContext :: C.Context 
toolTipContext = sharedToolTipPtrCntx <> toolTipCntx

instance Creatable (Ptr ToolTip) where 
  type CreationOptions (Ptr ToolTip) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| ToolTip* { new ToolTip( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(ToolTip* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement] ''ToolTip

instance UIElem ToolTip where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = ToolTip::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "ToolTip"