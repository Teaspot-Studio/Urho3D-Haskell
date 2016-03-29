{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Component(
    Component
  , componentContext
  , SharedComponent
  , SharedComponentPtr
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Scene.Internal.Component
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> componentCntx <> sharedComponentPtrCntx <> contextContext <> stringHashContext)
C.include "<Urho3D/Scene/Component.h>"
C.using "namespace Urho3D" 

componentContext :: C.Context 
componentContext = sharedComponentPtrCntx <> componentCntx <> stringHashContext

newComponent :: Ptr Context -> IO (Ptr Component)
newComponent ptr = [C.exp| Component* { new Component($(Context* ptr)) } |]

deleteComponent :: Ptr Component -> IO ()
deleteComponent ptr = [C.exp| void { delete $(Component* ptr) } |]

instance Createable (Ptr Component) where 
  type CreationOptions (Ptr Component) = Ptr Context 

  newObject = liftIO . newComponent
  deleteObject = liftIO . deleteComponent

sharedPtr "Component" 