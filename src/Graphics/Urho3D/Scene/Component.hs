{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Scene.Component(
    Component
  , componentContext
  , SharedComponent
  , WeakComponent
  , VectorSharedComponentPtr
  , VectorWeakComponentPtr
  , PODVectorComponentPtr
  , NodeComponent(..)
  , componentSetEnabled
  , componentRemove
  , componentGetID
  , componentGetNode
  , componentGetScene
  , componentIsEnabled
  , componentIsEnabledEffective
  , componentGetComponentByHash
  , componentGetComponent
  , componentGetComponentsByHash
  , componentGetComponents
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Maybe
import Data.Monoid
import Data.Proxy
import Foreign
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Scene.Internal.Component
import Graphics.Urho3D.Scene.Internal.Node
import Graphics.Urho3D.Scene.Internal.Scene
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Parent

C.context (C.cppCtx
  <> componentCntx
  <> sharedComponentPtrCntx
  <> contextContext
  <> stringHashContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> podVectorComponentPtrCntx
  <> weakComponentPtrCntx
  <> nodeCntx
  <> sceneCntx
  )
C.include "<Urho3D/Scene/Component.h>"
C.using "namespace Urho3D"

C.verbatim "typedef Vector<SharedPtr<Component> > VectorSharedComponentPtr;"
C.verbatim "typedef Vector<WeakPtr<Component> > VectorWeakComponentPtr;"
C.verbatim "typedef PODVector<Component*> PODVectorComponentPtr;"

componentContext :: C.Context
componentContext = sharedComponentPtrCntx
  <> componentCntx
  <> stringHashContext
  <> podVectorComponentPtrCntx
  <> weakComponentPtrCntx

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

class Parent Component a => NodeComponent a where
  nodeComponentType :: Proxy a -> StringHash

instance NodeComponent Component where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Component::GetTypeStatic().Value() } |]

-- | Set enabled/disabled state.
-- void SetEnabled(bool enable);
componentSetEnabled :: (Parent Component a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> Bool -- ^ enable
  -> m ()
componentSetEnabled p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(Component* ptr)->SetEnabled($(int v') != 0) } |]

-- | Remove from the scene node. If no other shared pointer references exist, causes immediate deletion.
-- void Remove();
componentRemove :: (Parent Component a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> m ()
componentRemove p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Component* ptr)->Remove() } |]

-- | Return ID.
-- unsigned GetID() const { return id_; }
componentGetID :: (Parent Component a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> m Word
componentGetID p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Component* ptr)->GetID() } |]

-- | Return scene node.
-- Node* GetNode() const { return node_; }
componentGetNode :: (Parent Component a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> m (Ptr Node)
componentGetNode p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Node* { $(Component* ptr)->GetNode() } |]

-- | Return the scene the node belongs to.
-- Scene* GetScene() const;
componentGetScene :: (Parent Component a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> m (Ptr Scene)
componentGetScene p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Scene* { $(Component* ptr)->GetScene() } |]

-- | Return whether is enabled.
-- bool IsEnabled() const { return enabled_; }
componentIsEnabled :: (Parent Component a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> m Bool
componentIsEnabled p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Component* ptr)->IsEnabled() } |]

-- | Return whether is effectively enabled (node is also enabled.)
-- bool IsEnabledEffective() const;
componentIsEnabledEffective :: (Parent Component a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> m Bool
componentIsEnabledEffective p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Component* ptr)->IsEnabledEffective() } |]

-- | Return component in the same scene node by type. If there are several, returns the first.
-- Component* GetComponent(StringHash type) const;
componentGetComponentByHash :: (Parent Component a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> StringHash -- ^ Hash of component
  -> m (Maybe (Ptr Component))
componentGetComponentByHash p h = liftIO $ do
  let ptr = parentPointer p
      h' = fromIntegral . stringHashValue $ h
  wrapNullPtr <$> [C.exp| Component* { $(Component* ptr)->GetComponent(StringHash($(unsigned int h'))) } |]

-- | Template version of returning a component in the same scene node by type.
-- template <class T> T* GetComponent() const;
componentGetComponent :: forall a ptr c m .(Parent Component a, Pointer ptr a, NodeComponent c, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> m (Maybe (Ptr c))
componentGetComponent p = liftIO $ do
  let h = nodeComponentType (Proxy :: Proxy c)
  mres <- componentGetComponentByHash p h
  pure . join $ castToChild <$> mres

-- | Return components in the same scene node by type.
-- void GetComponents(PODVector<Component*>& dest, StringHash type) const;
componentGetComponentsByHash :: (Parent Component a, Pointer ptr a, MonadIO m, ForeignVector v (Ptr Component))
  => ptr -- ^ Pointer to component or ascentor
  -> StringHash -- ^ Hash of component
  -> m (v (Ptr Component))
componentGetComponentsByHash p h = liftIO $ do
  let ptr = parentPointer p
      h' = fromIntegral . stringHashValue $ h
  vptr <- [C.block| PODVectorComponentPtr* {
    PODVectorComponentPtr* vec = new PODVectorComponentPtr();
    $(Component* ptr)->GetComponents(*vec, StringHash($(unsigned int h')));
    return vec;
  }
  |]
  v <- peekForeignVectorAs' vptr
  [C.exp| void {delete $(PODVectorComponentPtr* vptr)} |]
  pure v

-- | Template version of returning components in the same scene node by type.
-- template <class T> void GetComponents(PODVector<T*>& dest) const;
componentGetComponents :: forall a ptr c m . (Parent Component a, Pointer ptr a, NodeComponent c, MonadIO m)
  => ptr -- ^ Pointer to component or ascentor
  -> m [Ptr c]
componentGetComponents p = liftIO $ do
  let h = nodeComponentType (Proxy :: Proxy c)
  vres <- componentGetComponentsByHash p h
  pure . catMaybes $ castToChild <$> vres
