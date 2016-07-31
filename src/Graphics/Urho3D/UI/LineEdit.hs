{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.LineEdit(
    LineEdit 
  , lineEditContext
  , SharedLineEdit
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.LineEdit
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
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> sharedLineEditPtrCntx <> lineEditCntx <> contextContext <> uiElementContext <> borderImageContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/UI/LineEdit.h>"
C.using "namespace Urho3D"

lineEditContext :: C.Context 
lineEditContext = sharedLineEditPtrCntx <> lineEditCntx

instance Creatable (Ptr LineEdit) where 
  type CreationOptions (Ptr LineEdit) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| LineEdit* { new LineEdit( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(LineEdit* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''LineEdit

instance UIElem LineEdit where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = LineEdit::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "LineEdit"