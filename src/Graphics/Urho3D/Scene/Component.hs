{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Component(
    Component
  , componentContext
  , SharedComponent
  , WeakComponent
  , VectorSharedComponentPtr
  , VectorWeakComponentPtr
  , PODVectorComponentPtr
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Scene.Internal.Component
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Parent 

C.context (C.cppCtx <> componentCntx <> sharedComponentPtrCntx <> contextContext <> stringHashContext <> animatableContext <> serializableContext <> objectContext <> podVectorComponentPtrCntx <> weakComponentPtrCntx)
C.include "<Urho3D/Scene/Component.h>"
C.using "namespace Urho3D" 

C.verbatim "typedef Vector<SharedPtr<Component> > VectorSharedComponentPtr;"
C.verbatim "typedef Vector<WeakPtr<Component> > VectorWeakComponentPtr;"

componentContext :: C.Context 
componentContext = sharedComponentPtrCntx <> componentCntx <> stringHashContext <> podVectorComponentPtrCntx <> weakComponentPtrCntx

newComponent :: Ptr Context -> IO (Ptr Component)
newComponent ptr = [C.exp| Component* { new Component($(Context* ptr)) } |]

deleteComponent :: Ptr Component -> IO ()
deleteComponent ptr = [C.exp| void { delete $(Component* ptr) } |]

instance Creatable (Ptr Component) where 
  type CreationOptions (Ptr Component) = Ptr Context 

  newObject = liftIO . newComponent
  deleteObject = liftIO . deleteComponent

sharedPtr "Component" 
sharedWeakPtr "Component" 
podVectorPtr "Component"
deriveParents [''Object, ''Serializable, ''Animatable] ''Component

instance Creatable (Ptr VectorSharedComponentPtr) where 
  type CreationOptions (Ptr VectorSharedComponentPtr) = ()

  newObject _ = liftIO [C.exp| VectorSharedComponentPtr* { new VectorSharedComponentPtr() } |]
  deleteObject ptr = liftIO $ [C.exp| void {delete $(VectorSharedComponentPtr* ptr)} |]

instance ReadableVector VectorSharedComponentPtr where 
  type ReadVecElem VectorSharedComponentPtr = SharedPtr Component
  foreignVectorLength ptr = fromIntegral <$>
    liftIO [C.exp| unsigned int {$(VectorSharedComponentPtr* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do 
    let i' = fromIntegral i 
    peekSharedPtr =<< [C.exp| SharedComponent* { new SharedComponent((*$(VectorSharedComponentPtr* ptr))[$(int i')]) } |]

instance WriteableVector VectorSharedComponentPtr where 
  type WriteVecElem VectorSharedComponentPtr = SharedPtr Component
  foreignVectorAppend ptr sp = liftIO $ do 
    let p = pointer sp
    [C.exp| void { $(VectorSharedComponentPtr* ptr)->Push(SharedPtr<Component>($(Component* p))) } |]

instance Creatable (Ptr VectorWeakComponentPtr) where 
  type CreationOptions (Ptr VectorWeakComponentPtr) = ()

  newObject _ = liftIO [C.exp| VectorWeakComponentPtr* { new VectorWeakComponentPtr() } |]
  deleteObject ptr = liftIO $ [C.exp| void {delete $(VectorWeakComponentPtr* ptr)} |]

instance ReadableVector VectorWeakComponentPtr where 
  type ReadVecElem VectorWeakComponentPtr = WeakPtr Component
  foreignVectorLength ptr = fromIntegral <$>
    liftIO [C.exp| unsigned int {$(VectorWeakComponentPtr* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do 
    let i' = fromIntegral i 
    peekWeakPtr =<< [C.exp| WeakComponent* { new WeakComponent((*$(VectorWeakComponentPtr* ptr))[$(int i')]) } |]

instance WriteableVector VectorWeakComponentPtr where 
  type WriteVecElem VectorWeakComponentPtr = WeakPtr Component
  foreignVectorAppend ptr sp = liftIO $ do 
    let p = pointer sp
    [C.exp| void { $(VectorWeakComponentPtr* ptr)->Push(WeakPtr<Component>($(Component* p))) } |]