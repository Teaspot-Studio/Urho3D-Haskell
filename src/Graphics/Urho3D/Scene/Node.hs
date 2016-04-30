{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Node(
    Node
  , nodeContext
  , SharedNode
  , SharedNodePtr
  , VectorSharedNodePtr
  , PODVectorNodePtr
  , CreateMode(..)
  , TransformSpace(..)
  , NodeComponent(..)
  -- | Setters
  , nodeSetName
  , nodeSetPosition
  , nodeSetPosition2D
  , nodeSetPosition2D'
  , nodeSetRotation
  , nodeSetRotation2D
  , nodeSetDirection
  , nodeSetScale
  , nodeSetScale'
  , nodeSetScale2D
  , nodeSetScale2D'
  , nodeSetTransform
  , nodeSetTransformAndScale
  , nodeSetTransformAndScale'
  , nodeSetTransform2D
  , nodeSetTransformAndScale2D
  , nodeSetTransformAndScale2D'
  , nodeSetWorldPosition
  , nodeSetWorldPosition2D
  , nodeSetWorldPosition2D'
  , nodeSetWorldRotation
  , nodeSetWorldRotation2D
  , nodeSetWorldDirection
  , nodeSetWorldScale
  , nodeSetWorldScale'
  , nodeSetWorldScale2D
  , nodeSetWorldScale2D'
  , nodeSetWorldTransform
  , nodeSetWorldTransformAndScale
  , nodeSetWorldTransformAndScale'
  , nodeSetWorldTransform2D
  , nodeSetWorldTransformAndScale2D
  , nodeSetWorldTransformAndScale2D'
  , nodeTranslate
  , nodeTranslate2D
  , nodeRotate
  , nodeRotate2D
  , nodeRotateAround
  , nodeRotateAround2D
  , nodePitch
  , nodeYaw
  , nodeRoll
  , nodeLookAt
  , nodeLookAtSimple
  , nodeScaleUniform
  , nodeScale
  , nodeScale2D
  , nodeSetEnabled
  , nodeSetDeepEnabled
  , nodeResetDeepEnabled
  , nodeSetEnabledRecursive
  , nodeSetOwner
  , nodeMarkDirty
  , nodeCreateChild
  , nodeCreateChildSimple
  , nodeAddChild
  , nodeRemoveChild
  , nodeRemoveAllChildren
  , nodeRemoveChildren
  , nodeCreateComponent
  , nodeCreateCustomComponent
  , nodeGetOrCreateComponent
  , nodeCloneComponent
  , nodeCloneComponent'
  , nodeRemoveComponent
  , nodeRemoveComponent'
  , nodeRemoveComponents
  , nodeRemoveComponents'
  , nodeRemoveAllComponents
  , nodeClone 
  , nodeRemove
  , nodeSetParent
  , nodeSetVar 
  , nodeAddListener
  , nodeRemoveListener
  -- | Getters
  , nodeGetID
  , nodeGetName 
  , nodeGetNameHash
  , nodeGetTags
  , nodeHasTag
  , nodeGetParent
  , nodeGetScene
  , nodeIsEnabled
  , nodeIsEnabledSelf
  , nodeGetOwner
  , nodeGetPosition
  , nodeGetPosition2D
  , nodeGetRotation
  , nodeGetRotation2D
  , nodeGetDirection
  , nodeGetUp
  , nodeGetRight
  , nodeGetScale
  , nodeGetScale2D
  , nodeGetTransform
  , nodeGetWorldPosition
  , nodeGetWorldPosition2D
  , nodeGetWorldRotation
  , nodeGetWorldRotation2D
  , nodeGetWorldDirection
  , nodeGetWorldUp
  , nodeGetWorldRight
  , nodeGetWorldScale
  , nodeGetWorldScale2D
  , nodeGetWorldTransform
  -- Stoped here
  , NodeLocalToWorld(..)
  , nodeLocalToWorld2D
  , NodeWorldToLocal(..)
  , nodeWorldToLocal2D
  , nodeIsDirty
  , nodeGetNumChildren
  , nodeGetChildren 
  , nodeGetChildren'
  , nodeGetChildrenWithComponent
  , nodeGetChildrenWithTag 
  , nodeGetChildByIndex
  , nodeGetChildByName
  , nodeGetChildByNameHash
  , nodeGetNumComponents
  , nodeGetNumNetworkComponents
  , nodeGetComponents 
  , nodeGetComponentsByType
  , nodeGetComponent'
  , nodeGetComponent
  , nodeGetParentComponent
  , nodeHasComponent
  , nodeGetListeners
  , nodeGetVar
  , nodeGetVars
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Data.Proxy
import Foreign 
import Foreign.C.String
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.Quaternion
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Math.Vector4
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Internal.Scene
import Graphics.Urho3D.Scene.Internal.Node
import Graphics.Urho3D.Network.Connection
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx 
  <> nodeCntx 
  <> sharedNodePtrCntx 
  <> contextContext 
  <> stringHashContext 
  <> componentContext 
  <> quaternionContext 
  <> vector2Context 
  <> vector3Context 
  <> vector4Context 
  <> connectionContext 
  <> variantContext 
  <> stringContext 
  <> sceneCntx 
  <> matrix3x4Context
  <> podVectorNodePtrCntx)

C.include "<Urho3D/Scene/Node.h>"
C.include "<Urho3D/Scene/Component.h>"
C.using "namespace Urho3D" 

C.verbatim "typedef Vector<SharedPtr<Node> > VectorSharedNodePtr;"
C.verbatim "typedef Vector<SharedPtr<Component> > VectorSharedComponentPtr;"
C.verbatim "typedef Vector<WeakPtr<Component> > VectorSharedWeakComponentPtr;"
C.verbatim "typedef PODVector<Node*> PODVectorNodePtr;"
C.verbatim "typedef PODVector<Component*> PODVectorComponentPtr;"
C.verbatim "typedef VariantMap HashMapStringHashVariant;"

nodeContext :: C.Context 
nodeContext = sharedNodePtrCntx <> nodeCntx <> stringHashContext <> componentContext <> podVectorNodePtrCntx

newNode :: Ptr Context -> IO (Ptr Node)
newNode ptr = [C.exp| Node* { new Node($(Context* ptr)) } |]

deleteNode :: Ptr Node -> IO ()
deleteNode ptr = [C.exp| void { delete $(Node* ptr) } |]

instance Createable (Ptr Node) where 
  type CreationOptions (Ptr Node) = Ptr Context 

  newObject = liftIO . newNode
  deleteObject = liftIO . deleteNode

sharedPtr "Node" 
podVectorPtr "Node"

-- | Component and child node creation mode for networking.
data CreateMode = 
    CM'Replicated
  | CM'Local 
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Transform space for translations and rotations.
data TransformSpace =
    TS'Local
  | TS'Parent 
  | TS'World 
  deriving (Eq, Ord, Show, Bounded, Enum)

class Parent Component a => NodeComponent a where 
  nodeComponentType :: Proxy a -> Ptr StringHash 

instance NodeComponent Component where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = Component::GetTypeStatic();
    return &h;
  } |]

instance Createable (Ptr VectorSharedNodePtr) where 
  type CreationOptions (Ptr VectorSharedNodePtr) = ()

  newObject _ = liftIO [C.exp| VectorSharedNodePtr* { new VectorSharedNodePtr() } |]
  deleteObject ptr = liftIO $ [C.exp| void {delete $(VectorSharedNodePtr* ptr)} |]

instance ReadableVector VectorSharedNodePtr where 
  type ReadVecElem VectorSharedNodePtr = SharedNodePtr
  foreignVectorLength ptr = fromIntegral <$>
    liftIO [C.exp| unsigned int {$(VectorSharedNodePtr* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do 
    let i' = fromIntegral i 
    wrapSharedNodePtr =<< [C.exp| SharedNode* { new SharedPtr<Node>((*$(VectorSharedNodePtr* ptr))[$(int i')]) } |]

instance WriteableVector VectorSharedNodePtr where 
  type WriteVecElem VectorSharedNodePtr = SharedNodePtr 
  foreignVectorAppend ptr sp = liftIO $ do 
    let p = pointer sp
    [C.exp| void { $(VectorSharedNodePtr* ptr)->Push(SharedPtr<Node>($(Node* p))) } |]

-- | Set name of the scene node. Names are not required to be unique.
nodeSetName :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> String -- ^ Name
  -> m ()
nodeSetName p str = liftIO $ withCString str $ \str' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetName(String($(const char* str'))) } |]

-- | Set position in parent space. If the scene node is on the root level (is child of the scene itself), this is same as world space.
nodeSetPosition :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Value
  -> m ()
nodeSetPosition p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetPosition(*$(Vector3* v')) } |]

-- | Set position in parent space (for Urho2D).
nodeSetPosition2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Value
  -> m ()
nodeSetPosition2D p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetPosition2D(*$(Vector2* v')) } |]

-- | Set position in parent space (for Urho2D).
nodeSetPosition2D' :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Float -- ^ X 
  -> Float -- ^ Y
  -> m ()
nodeSetPosition2D' p xv yv = liftIO $ do 
  let ptr = parentPointer p 
      x' = realToFrac xv 
      y' = realToFrac yv
  [C.exp| void { $(Node* ptr)->SetPosition2D($(float x'), $(float y')) } |]

-- | Sets node rotation with quaternion
nodeSetRotation :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Quaternion -- ^ Rotation
  -> m ()
nodeSetRotation p q = liftIO $ with q $ \q' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Node* ptr)->SetRotation(*$(Quaternion* q'))} |]

-- | Set rotation in parent space (for Urho2D).
nodeSetRotation2D :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Float -- ^ Rotation
  -> m ()
nodeSetRotation2D p a = liftIO $ do 
  let ptr = parentPointer p 
      a' = realToFrac a
  [C.exp| void {$(Node* ptr)->SetRotation2D($(float a'))} |]

-- | Set forward direction in parent space. Positive Z axis equals identity rotation.
nodeSetDirection :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Value
  -> m ()
nodeSetDirection p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetDirection(*$(Vector3* v')) } |]

-- | Set scale in parent space.
nodeSetScale :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Scale factor by each axis
  -> m ()
nodeSetScale p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetScale(*$(Vector3* v')) } |]

-- | Set uniform scale in parent space.
nodeSetScale' :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Float -- ^ Scale factor
  -> m ()
nodeSetScale' p a = liftIO $ do 
  let ptr = parentPointer p 
      a' = realToFrac a
  [C.exp| void {$(Node* ptr)->SetScale($(float a'))} |]

-- | Set scale in parent space (for Urho2D).
nodeSetScale2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Scale factor by each axis
  -> m ()
nodeSetScale2D p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetScale2D(*$(Vector2* v')) } |]

-- | Set scale in parent space (for Urho2D).
nodeSetScale2D' :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Float -- ^ X factor
  -> Float -- ^ Y factor
  -> m ()
nodeSetScale2D' p xv yv = liftIO $ do 
  let ptr = parentPointer p 
      x' = realToFrac xv 
      y' = realToFrac yv
  [C.exp| void { $(Node* ptr)->SetScale2D($(float x'), $(float y')) } |]

-- | Set both position and rotation in parent space as an atomic operation. This is faster than setting position and rotation separately.
nodeSetTransform :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Position
  -> Quaternion -- ^ Rotation
  -> m ()
nodeSetTransform p v q = liftIO $ with v $ \v' -> with q $ \q' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetTransform(*$(Vector3* v'), *$(Quaternion* q')) } |]

-- | Set both position, rotation and scale in parent space as an atomic operation.
nodeSetTransformAndScale :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Position
  -> Quaternion -- ^ Rotation
  -> Vector3 -- ^ Scale
  -> m ()
nodeSetTransformAndScale p v q s = liftIO $ with v $ \v' -> with q $ \q' -> with s $ \s' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetTransform(*$(Vector3* v'), *$(Quaternion* q'), *$(Vector3* s')) } |]

-- | Set both position, rotation and uniform scale in parent space as an atomic operation.
nodeSetTransformAndScale' :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Position
  -> Quaternion -- ^ Rotation
  -> Float -- ^ Scale
  -> m ()
nodeSetTransformAndScale' p v q s = liftIO $ with v $ \v' -> with q $ \q' -> do 
  let ptr = parentPointer p 
      s' = realToFrac s
  [C.exp| void { $(Node* ptr)->SetTransform(*$(Vector3* v'), *$(Quaternion* q'), $(float s')) } |]

-- | Set both position and rotation in parent space as an atomic operation (for Urho2D).
nodeSetTransform2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Position
  -> Float -- ^ Rotation
  -> m ()
nodeSetTransform2D p v q = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
      q' = realToFrac q
  [C.exp| void { $(Node* ptr)->SetTransform2D(*$(Vector2* v'), $(float q')) } |]

-- | Set both position, rotation and scale in parent space as an atomic operation (for Urho2D).
nodeSetTransformAndScale2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Position
  -> Float -- ^ Rotation
  -> Vector2 -- ^ Scale
  -> m ()
nodeSetTransformAndScale2D p v q s = liftIO $ with v $ \v' -> with s $ \s' -> do 
  let ptr = parentPointer p 
      q' = realToFrac q
  [C.exp| void { $(Node* ptr)->SetTransform2D(*$(Vector2* v'), $(float q'), *$(Vector2* s')) } |]

-- | Set both position, rotation and uniform scale in parent space as an atomic operation (for Urho2D).
nodeSetTransformAndScale2D' :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Position
  -> Float -- ^ Rotation
  -> Float -- ^ Scale
  -> m ()
nodeSetTransformAndScale2D' p v q s = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
      q' = realToFrac q
      s' = realToFrac s
  [C.exp| void { $(Node* ptr)->SetTransform2D(*$(Vector2* v'), $(float q'), $(float s')) } |]

-- | Set position in world space. If the scene node is on the root level (is child of the scene itself), this is same as world space.
nodeSetWorldPosition :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Value
  -> m ()
nodeSetWorldPosition p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetWorldPosition(*$(Vector3* v')) } |]

-- | Set position in world space (for Urho2D).
nodeSetWorldPosition2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Value
  -> m ()
nodeSetWorldPosition2D p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetWorldPosition2D(*$(Vector2* v')) } |]

-- | Set position in world space (for Urho2D).
nodeSetWorldPosition2D' :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Float -- ^ X 
  -> Float -- ^ Y
  -> m ()
nodeSetWorldPosition2D' p xv yv = liftIO $ do 
  let ptr = parentPointer p 
      x' = realToFrac xv 
      y' = realToFrac yv
  [C.exp| void { $(Node* ptr)->SetWorldPosition2D($(float x'), $(float y')) } |]

-- | Sets node rotation with quaternion
nodeSetWorldRotation :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Quaternion -- ^ Rotation
  -> m ()
nodeSetWorldRotation p q = liftIO $ with q $ \q' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Node* ptr)->SetWorldRotation(*$(Quaternion* q'))} |]

-- | Set rotation in world space (for Urho2D).
nodeSetWorldRotation2D :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Float -- ^ Rotation
  -> m ()
nodeSetWorldRotation2D p a = liftIO $ do 
  let ptr = parentPointer p 
      a' = realToFrac a
  [C.exp| void {$(Node* ptr)->SetWorldRotation2D($(float a'))} |]

-- | Set forward direction in world space. Positive Z axis equals identity rotation.
nodeSetWorldDirection :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Value
  -> m ()
nodeSetWorldDirection p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetWorldDirection(*$(Vector3* v')) } |]

-- | Set scale in world space.
nodeSetWorldScale :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Scale factor by each axis
  -> m ()
nodeSetWorldScale p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetWorldScale(*$(Vector3* v')) } |]

-- | Set uniform scale in world space.
nodeSetWorldScale' :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Float -- ^ Scale factor
  -> m ()
nodeSetWorldScale' p a = liftIO $ do 
  let ptr = parentPointer p 
      a' = realToFrac a
  [C.exp| void {$(Node* ptr)->SetWorldScale($(float a'))} |]

-- | Set scale in world space (for Urho2D).
nodeSetWorldScale2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Scale factor by each axis
  -> m ()
nodeSetWorldScale2D p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetWorldScale2D(*$(Vector2* v')) } |]

-- | Set scale in world space (for Urho2D).
nodeSetWorldScale2D' :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Float -- ^ X factor
  -> Float -- ^ Y factor
  -> m ()
nodeSetWorldScale2D' p xv yv = liftIO $ do 
  let ptr = parentPointer p 
      x' = realToFrac xv 
      y' = realToFrac yv
  [C.exp| void { $(Node* ptr)->SetWorldScale2D($(float x'), $(float y')) } |]

-- | Set both position and rotation in world space as an atomic operation. This is faster than setting position and rotation separately.
nodeSetWorldTransform :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Position
  -> Quaternion -- ^ Rotation
  -> m ()
nodeSetWorldTransform p v q = liftIO $ with v $ \v' -> with q $ \q' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetWorldTransform(*$(Vector3* v'), *$(Quaternion* q')) } |]

-- | Set both position, rotation and scale in world space as an atomic operation.
nodeSetWorldTransformAndScale :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Position
  -> Quaternion -- ^ Rotation
  -> Vector3 -- ^ Scale
  -> m ()
nodeSetWorldTransformAndScale p v q s = liftIO $ with v $ \v' -> with q $ \q' -> with s $ \s' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->SetWorldTransform(*$(Vector3* v'), *$(Quaternion* q'), *$(Vector3* s')) } |]

-- | Set both position, rotation and uniform scale in world space as an atomic operation.
nodeSetWorldTransformAndScale' :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ Position
  -> Quaternion -- ^ Rotation
  -> Float -- ^ Scale
  -> m ()
nodeSetWorldTransformAndScale' p v q s = liftIO $ with v $ \v' -> with q $ \q' -> do 
  let ptr = parentPointer p 
      s' = realToFrac s
  [C.exp| void { $(Node* ptr)->SetWorldTransform(*$(Vector3* v'), *$(Quaternion* q'), $(float s')) } |]

-- | Set both position and rotation in world space as an atomic operation (for Urho2D).
nodeSetWorldTransform2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Position
  -> Float -- ^ Rotation
  -> m ()
nodeSetWorldTransform2D p v q = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
      q' = realToFrac q
  [C.exp| void { $(Node* ptr)->SetWorldTransform2D(*$(Vector2* v'), $(float q')) } |]

-- | Set both position, rotation and scale in world space as an atomic operation (for Urho2D).
nodeSetWorldTransformAndScale2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Position
  -> Float -- ^ Rotation
  -> Vector2 -- ^ Scale
  -> m ()
nodeSetWorldTransformAndScale2D p v q s = liftIO $ with v $ \v' -> with s $ \s' -> do 
  let ptr = parentPointer p 
      q' = realToFrac q
  [C.exp| void { $(Node* ptr)->SetWorldTransform2D(*$(Vector2* v'), $(float q'), *$(Vector2* s')) } |]

-- | Set both position, rotation and uniform scale in world space as an atomic operation (for Urho2D).
nodeSetWorldTransformAndScale2D' :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ Position
  -> Float -- ^ Rotation
  -> Float -- ^ Scale
  -> m ()
nodeSetWorldTransformAndScale2D' p v q s = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
      q' = realToFrac q
      s' = realToFrac s
  [C.exp| void { $(Node* ptr)->SetWorldTransform2D(*$(Vector2* v'), $(float q'), $(float s')) } |]

-- | Move the scene node in the chosen transform space.
nodeTranslate :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ delta
  -> TransformSpace -- ^ space
  -> m ()
nodeTranslate p v s = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
      s' = fromIntegral $ fromEnum s
  [C.exp| void { $(Node* ptr)->Translate(*$(Vector3* v'), (TransformSpace)$(int s')) } |]

-- | Move the scene node in the chosen transform space.
nodeTranslate2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ delta
  -> TransformSpace -- ^ space
  -> m ()
nodeTranslate2D p v s = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
      s' = fromIntegral $ fromEnum s
  [C.exp| void { $(Node* ptr)->Translate2D(*$(Vector2* v'), (TransformSpace)$(int s')) } |]

-- | Rotate the scene node in the chosen transform space.
nodeRotate :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Quaternion -- ^ delta
  -> TransformSpace -- ^ space
  -> m ()
nodeRotate p q s = liftIO $ with q $ \q' -> do 
  let ptr = parentPointer p 
      s' = fromIntegral $ fromEnum s
  [C.exp| void { $(Node* ptr)->Rotate(*$(Quaternion* q'), (TransformSpace)$(int s')) } |]

-- | Rotate the scene node in the chosen transform space (for Urho2D).
nodeRotate2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Float -- ^ delta
  -> TransformSpace -- ^ space
  -> m ()
nodeRotate2D p q s = liftIO $ do 
  let ptr = parentPointer p 
      q' = realToFrac q
      s' = fromIntegral $ fromEnum s
  [C.exp| void { $(Node* ptr)->Rotate2D($(float q'), (TransformSpace)$(int s')) } |]

-- | Rotate around a point in the chosen transform space.
nodeRotateAround :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector3 -- ^ point
  -> Quaternion -- ^ delta
  -> TransformSpace -- ^ space
  -> m ()
nodeRotateAround p pv q s = liftIO $ with pv $ \pv' -> with q $ \q' -> do 
  let ptr = parentPointer p 
      s' = fromIntegral $ fromEnum s
  [C.exp| void { $(Node* ptr)->RotateAround(*$(Vector3* pv'), *$(Quaternion* q'), (TransformSpace)$(int s')) } |]

-- | Rotate around a point in the chosen transform space.
nodeRotateAround2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or child
  -> Vector2 -- ^ point
  -> Float -- ^ delta
  -> TransformSpace -- ^ space
  -> m ()
nodeRotateAround2D p pv q s = liftIO $ with pv $ \pv' -> do 
  let ptr = parentPointer p 
      q' = realToFrac q
      s' = fromIntegral $ fromEnum s
  [C.exp| void { $(Node* ptr)->RotateAround2D(*$(Vector2* pv'), $(float q'), (TransformSpace)$(int s')) } |]


-- | Rotate around the X axis
nodePitch :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Float -- ^ angle 
  -> TransformSpace -- ^ space
  -> m ()
nodePitch p angle space = liftIO $ do 
  let ptr = parentPointer p 
      a  = realToFrac angle 
      sp = fromIntegral $ fromEnum space 
  [C.exp| void { $(Node* ptr)->Pitch($(float a), (TransformSpace)$(int sp)) } |]

-- | Rotate around the Y axis
nodeYaw :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Float -- ^ angle 
  -> TransformSpace -- ^ space
  -> m ()
nodeYaw p angle space = liftIO $ do 
  let ptr = parentPointer p 
      a  = realToFrac angle 
      sp = fromIntegral $ fromEnum space 
  [C.exp| void { $(Node* ptr)->Yaw($(float a), (TransformSpace)$(int sp)) } |]

-- | Rotate around the Z axis
nodeRoll :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Float -- ^ angle 
  -> TransformSpace -- ^ space
  -> m ()
nodeRoll p angle space = liftIO $ do 
  let ptr = parentPointer p 
      a  = realToFrac angle 
      sp = fromIntegral $ fromEnum space 
  [C.exp| void { $(Node* ptr)->Roll($(float a), (TransformSpace)$(int sp)) } |]

-- | Look at a target position in the chosen transform space. Note that the up vector is always specified in world space. Return true if successful, or false if resulted in an illegal rotation, in which case the current rotation remains.
nodeLookAt :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Vector3 -- ^ target
  -> Vector3 -- ^ up
  -> TransformSpace -- ^ space
  -> m Bool
nodeLookAt p target up space = liftIO $ with target $ \t -> with up $ \u -> do 
  let ptr = parentPointer p 
      sp = fromIntegral $ fromEnum space 
  toBool <$> [C.exp| int { (int)$(Node* ptr)->LookAt(*$(Vector3* t), *$(Vector3* u), (TransformSpace)$(int sp)) } |]

-- | Look at a target position in the chosen transform space. Note that the up vector is always specified in world space. Return true if successful, or false if resulted in an illegal rotation, in which case the current rotation remains.
nodeLookAtSimple :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Vector3 -- ^ target
  -> m Bool
nodeLookAtSimple p target = nodeLookAt p target vec3Up TS'World 

-- | Modify scale in parent space uniformly.
nodeScaleUniform :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Float -- ^ scale 
  -> m ()
nodeScaleUniform p scale = liftIO $ do 
  let ptr = parentPointer p 
      v  = realToFrac scale 
  [C.exp| void { $(Node* ptr)->Scale($(float v)) } |]

-- | Modify scale in parent space.
nodeScale :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Vector3-- ^ scale 
  -> m ()
nodeScale p scale = liftIO $ with scale $ \v -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->Scale(*$(Vector3* v)) } |]

-- | Modify scale in parent space (for Urho2D).
nodeScale2D :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Vector2-- ^ scale 
  -> m ()
nodeScale2D p scale = liftIO $ with scale $ \v -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->Scale2D(*$(Vector2* v)) } |]

-- | Set enabled/disabled state without recursion. Components in a disabled node become effectively disabled regardless of their own enable/disable state.
nodeSetEnabled :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Bool -- ^ enable 
  -> m ()
nodeSetEnabled p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = fromBool v 
  [C.exp| void { $(Node* ptr)->SetEnabled($(int v') != 0) } |]

-- | Set enabled state on self and child nodes. Nodes' own enabled state is remembered (IsEnabledSelf) and can be restored.
nodeSetDeepEnabled :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Bool -- ^ enable 
  -> m ()
nodeSetDeepEnabled p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = fromBool v 
  [C.exp| void { $(Node* ptr)->SetDeepEnabled($(int v') != 0) } |]

-- | Reset enabled state to the node's remembered state prior to calling SetDeepEnabled.
nodeResetDeepEnabled :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> m ()
nodeResetDeepEnabled p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->ResetDeepEnabled() } |]

-- | Set enabled state on self and child nodes. Unlike SetDeepEnabled this does not remember the nodes' own enabled state, but overwrites it.
nodeSetEnabledRecursive :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Bool -- ^ enable 
  -> m ()
nodeSetEnabledRecursive p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = fromBool v 
  [C.exp| void { $(Node* ptr)->SetEnabledRecursive($(int v') != 0) } |]

-- | Set owner connection for networking.
nodeSetOwner :: (Parent Node a, Pointer p a, Parent Connection conn, Pointer pConn conn, MonadIO m) => p -- ^ Node pointer or child
  -> pConn -- ^ owner 
  -> m ()
nodeSetOwner p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = parentPointer v 
  [C.exp| void { $(Node* ptr)->SetOwner($(Connection* v')) } |]

-- | Mark node and child nodes to need world transform recalculation. Notify listener components.
nodeMarkDirty :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> m ()
nodeMarkDirty p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->MarkDirty() } |]

-- | Create a child scene node (with specified ID if provided).
nodeCreateChild :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> String -- ^ name, default ""
  -> CreateMode -- ^ mode, default replicated 
  -> Int -- ^ id, default 0 
  -> m (Ptr Node)
nodeCreateChild p name mode i = liftIO $ withCString name $ \name' -> do 
  let ptr = parentPointer p 
      mode' = fromIntegral $ fromEnum mode 
      i' = fromIntegral i 
  [C.exp| Node* { $(Node* ptr)->CreateChild(String($(const char* name')), (CreateMode)$(int mode'), $(int i')) } |]

-- | Create a child scene node
nodeCreateChildSimple :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> m (Ptr Node)
nodeCreateChildSimple p = nodeCreateChild p "" CM'Replicated 0

-- | Add a child scene node at a specific index. If index is not explicitly specified or is greater than current children size, append the new child at the end.
nodeAddChild :: (Parent Node a, Pointer p a, Parent Node child, Pointer pChild child, MonadIO m) => p -- ^ Node pointer or child
  -> pChild -- ^ pointer to child node
  -> Maybe Int -- ^ maybe an index
  -> m ()
nodeAddChild p c mi = liftIO $ do 
  let ptr = parentPointer p 
      child = parentPointer c 
      i = maybe [C.pure| int { M_MAX_UNSIGNED } |] fromIntegral mi
  [C.exp| void { $(Node* ptr)->AddChild($(Node* child), $(int i)) } |]

-- | Remove a child scene node.
nodeRemoveChild :: (Parent Node a, Pointer p a, Parent Node child, Pointer pChild child, MonadIO m) => p -- ^ Node pointer or child
  -> pChild -- ^ pointer to child node
  -> m ()
nodeRemoveChild p c = liftIO $ do 
  let ptr = parentPointer p 
      child = parentPointer c 
  [C.exp| void { $(Node* ptr)->RemoveChild($(Node* child)) } |]

-- | Remove all child scene nodes
nodeRemoveAllChildren :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> m ()
nodeRemoveAllChildren p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->RemoveAllChildren() } |]

-- | Remove child scene nodes that match criteria
nodeRemoveChildren :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Bool -- ^ removeReplicated
  -> Bool -- ^ removeLocal
  -> Bool -- ^ recursive
  -> m ()
nodeRemoveChildren p removeReplicated removeLocal recursive = liftIO $ do 
  let ptr = parentPointer p 
      r' = fromBool removeReplicated
      l' = fromBool removeLocal
      rec' = fromBool recursive
  [C.exp| void { $(Node* ptr)->RemoveChildren($(int r') != 0, $(int l') != 0, $(int rec') != 0) } |]

-- | Create a component to this node (with specified ID if provided)
nodeCreateComponent :: forall a p c m . (Parent Node a, Pointer p a, NodeComponent c, MonadIO m)
  => p -- ^ Node pointer or child
  -> Maybe CreateMode -- ^ mode, default is replicated
  -> Maybe Int -- ^ id, default is 0 
  -> m (Maybe (Ptr c))
nodeCreateComponent p mc mi = liftIO $ do 
  let ptr = parentPointer p 
      c' = maybe [C.pure| int { (int)REPLICATED } |] (fromIntegral . fromEnum) mc 
      i' = maybe 0 fromIntegral mi
      th = nodeComponentType (Proxy :: Proxy c)
  cp <- [C.exp| Component* { $(Node* ptr)->CreateComponent(*$(StringHash* th), (CreateMode)$(int c'), $(int i')) } |]
  join <$> checkNullPtr' cp (return . castToChild)

-- | Create a component to this node (with specified ID if provided)
nodeCreateCustomComponent :: forall a p c m . (Parent Node a, Pointer p a, NodeComponent c, MonadIO m)
  => p -- ^ Node pointer or child
  -> ForeignPtr StringHash -- ^ Custom type
  -> Maybe CreateMode -- ^ mode, default is replicated
  -> Maybe Int -- ^ id, default is 0 
  -> m (Maybe (Ptr c))
nodeCreateCustomComponent p fct mc mi = liftIO $ withForeignPtr fct $ \ct -> do 
  let ptr = parentPointer p 
      c' = maybe [C.pure| int { (int)REPLICATED } |] (fromIntegral . fromEnum) mc 
      i' = maybe 0 fromIntegral mi
  cp <- [C.exp| Component* { $(Node* ptr)->CreateComponent(*$(StringHash* ct), (CreateMode)$(int c'), $(int i')) } |]
  join <$> checkNullPtr' cp (return . castToChild)

-- | Create a component to this node if it does not exist already.
nodeGetOrCreateComponent :: forall a p c m . (Parent Node a, Pointer p a, NodeComponent c, MonadIO m) 
  => p -- ^ Node pointer or child
  -> Maybe CreateMode -- ^ mode, default is replicated
  -> Maybe Int -- ^ id, default is 0 
  -> m (Maybe (Ptr c))
nodeGetOrCreateComponent p mc mi = liftIO $ do 
  let ptr = parentPointer p 
      c' = maybe [C.pure| int { (int)REPLICATED } |] (fromIntegral . fromEnum) mc 
      i' = maybe 0 fromIntegral mi 
      th = nodeComponentType (Proxy :: Proxy c)
  cp <- [C.exp| Component* { $(Node* ptr)->GetOrCreateComponent(*$(StringHash* th), (CreateMode)$(int c'), $(int i')) } |]
  join <$> checkNullPtr' cp (return . castToChild)

-- |  Clone a component from another node using its create mode. Return the clone if successful or null on failure.
nodeCloneComponent :: (Parent Node a, Pointer p a, Parent Component cmp, Pointer pComponent cmp, MonadIO m) => p -- ^ Node pointer or child
  -> pComponent -- ^ Component pointer 
  -> Maybe Int -- ^ id, default 0 
  -> m (Maybe (Ptr Component))
nodeCloneComponent p pCmp mi = liftIO $ do 
  let ptr = parentPointer p 
      cmp = parentPointer pCmp 
      i' = maybe 0 fromIntegral mi
  cp <- [C.exp| Component* { $(Node* ptr)->CloneComponent($(Component* cmp), $(int i')) } |]
  checkNullPtr' cp return

-- | Clone a component from another node and specify the create mode. Return the clone if successful or null on failure.
nodeCloneComponent' :: (Parent Node a, Pointer p a, Parent Component cmp, Pointer pComponent cmp, MonadIO m) => p -- ^ Node pointer or child
  -> pComponent -- ^ Component pointer 
  -> CreateMode -- ^ mode
  -> Maybe Int -- ^ id, default 0 
  -> m (Maybe (Ptr Component))
nodeCloneComponent' p pCmp cm mi = liftIO $ do 
  let ptr = parentPointer p 
      cmp = parentPointer pCmp 
      cm' = fromIntegral $ fromEnum cm
      i' = maybe 0 fromIntegral mi
  cp <- [C.exp| Component* { $(Node* ptr)->CloneComponent($(Component* cmp), (CreateMode)$(int cm'), $(int i')) } |]
  checkNullPtr' cp return

-- | Remove a component from this node.
nodeRemoveComponent :: (Parent Node a, Pointer p a, Parent Component cmp, Pointer pComponent cmp, MonadIO m) => p -- ^ Node pointer or child
  -> pComponent -- ^ Component pointer 
  -> m ()
nodeRemoveComponent p pCmp = liftIO $ do 
  let ptr = parentPointer p 
      cmp = parentPointer pCmp 
  [C.exp| void { $(Node* ptr)->RemoveComponent($(Component* cmp)) } |] 

-- | Remove the first component of specific type from this node.
nodeRemoveComponent' :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Ptr StringHash -- ^ type
  -> m ()
nodeRemoveComponent' p ph = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->RemoveComponent(*$(StringHash* ph)) } |]

-- | Remove components that match criteria.
nodeRemoveComponents :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Bool -- ^ removeReplicated
  -> Bool -- ^ removeLocal
  -> m ()
nodeRemoveComponents p removeReplicated removeLocal = liftIO $ do 
  let ptr = parentPointer p 
      r' = fromBool removeReplicated
      l' = fromBool removeLocal
  [C.exp| void { $(Node* ptr)->RemoveComponents($(int r') != 0, $(int l') != 0)} |]

-- | Remove all components of specific type.
nodeRemoveComponents' :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Ptr StringHash -- ^ type
  -> m ()
nodeRemoveComponents' p ph = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->RemoveComponents(*$(StringHash* ph)) } |]

-- | Remove all components from this node.
nodeRemoveAllComponents :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> m ()
nodeRemoveAllComponents p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(Node* ptr)->RemoveAllComponents() } |]

-- | Clone scene node, components and child nodes. Return the clone.
nodeClone :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> CreateMode 
  -> m (Ptr Node)
nodeClone p cm = liftIO $ do 
  let ptr = parentPointer p 
      cm' = fromIntegral . fromEnum $ cm
  [C.exp| Node* {$(Node* ptr)->Clone((CreateMode)$(int cm'))} |]

-- | Remove from the parent node. If no other shared pointer references exist, causes immediate deletion.
nodeRemove:: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m ()
nodeRemove p = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| void {$(Node* ptr)->Remove()} |]

-- | Set parent scene node. Retains the world transform.
nodeSetParent:: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> Ptr Node -- ^ parent
  -> m ()
nodeSetParent p parent = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| void {$(Node* ptr)->SetParent($(Node* parent))} |]

-- | Set a user variable.
nodeSetVar :: (Parent Node a, Pointer p a, VariantStorable v, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> String -- ^ key
  -> v -- ^ value
  -> m ()
nodeSetVar p key value = liftIO $ withObject key $ \pkey -> withVariant value $ \pvalue -> do 
  let ptr = parentPointer p
  [C.exp| void {$(Node* ptr)->SetVar(*$(StringHash* pkey), *$(Variant* pvalue))} |]

-- | Add listener component that is notified of node being dirtied. Can either be in the same node or another.
nodeAddListener:: (Parent Node a, Pointer p a, Parent Component b, Pointer pComponent b, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> pComponent -- ^ Pointer to component
  -> m ()
nodeAddListener p pcmp = liftIO $ do 
  let ptr = parentPointer p
      cmp = parentPointer pcmp
  [C.exp| void {$(Node* ptr)->AddListener($(Component* cmp))} |]

-- | Remove listener component.
nodeRemoveListener:: (Parent Node a, Pointer p a, Parent Component b, Pointer pComponent b, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> pComponent -- ^ Pointer to component
  -> m ()
nodeRemoveListener p pcmp = liftIO $ do 
  let ptr = parentPointer p
      cmp = parentPointer pcmp
  [C.exp| void {$(Node* ptr)->RemoveListener($(Component* cmp))} |]

-- | Return ID.
nodeGetID :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Word
nodeGetID p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| unsigned int {$(Node* ptr)->GetID()} |]

-- | Return name.
nodeGetName :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m String
nodeGetName p = liftIO $ do 
  let ptr = parentPointer p 
  loadConstUrhoString =<< [C.exp| const String* {&$(Node* ptr)->GetName()} |]

-- | Return name hash. You need to delete the hash.
nodeGetNameHash :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m (Ptr StringHash)
nodeGetNameHash p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| StringHash* {new StringHash($(Node* ptr)->GetNameHash())} |]

-- | Return all tags.
nodeGetTags :: (Parent Node a, Pointer p a, MonadIO m, ForeignVectorRepresent v)
  => p -- ^ Node pointer or pointer to ascentor
  -> m (v String) -- ^ Container with tags
nodeGetTags p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.exp| const StringVector* {&$(Node* ptr)->GetTags()} |]

-- | Return whether has a specific tag.
nodeHasTag :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> String -- ^ tag
  -> m Bool
nodeHasTag p tag = liftIO $ withCString tag $ \tag' -> do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int {(int)$(Node* ptr)->HasTag(String($(const char* tag')))} |]

-- | Return parent scene node.
nodeGetParent :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m (Ptr Node)
nodeGetParent p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Node* {$(Node* ptr)->GetParent()} |]

-- | Return scene.
nodeGetScene :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m (Ptr Scene)
nodeGetScene p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Scene* {$(Node* ptr)->GetScene()} |]

-- | Return whether is enabled. Disables nodes effectively disable all their components.
nodeIsEnabled :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Bool
nodeIsEnabled p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int {(int)$(Node* ptr)->IsEnabled()} |]

-- | Returns the node's last own enabled state. May be different than the value returned by IsEnabled when SetDeepEnabled has been used.
nodeIsEnabledSelf :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Bool
nodeIsEnabledSelf p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int {(int)$(Node* ptr)->IsEnabledSelf()} |]

-- | Return owner connection in networking.
nodeGetOwner :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m (Ptr Connection)
nodeGetOwner p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Connection* {$(Node* ptr)->GetOwner()} |]

-- | Return position in parent space.
nodeGetPosition :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetPosition p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const Vector3* {&$(Node* ptr)->GetPosition()} |]

-- | Return position in parent space (for Urho2D).
nodeGetPosition2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector2
nodeGetPosition2D p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector2* {new Vector2($(Node* ptr)->GetPosition2D())} |]
  v <- peek pvec 
  [C.exp| void {delete $(Vector2* pvec)} |]
  return v 

-- | Returns node rotation in quaternion
nodeGetRotation :: (Parent Node a, Pointer p a, MonadIO m) 
  => p -- ^ Node pointer or child
  -> m Quaternion
nodeGetRotation p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const Quaternion* { &$(Node* ptr)->GetRotation() } |]

-- | Return rotation in parent space (for Urho2D).
nodeGetRotation2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Float
nodeGetRotation2D p = liftIO $ do 
  let ptr = parentPointer p 
  realToFrac <$> [C.exp| float {$(Node* ptr)->GetRotation2D()} |]

-- | Return forward direction in parent space. Positive Z axis equals identity rotation.
nodeGetDirection :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetDirection p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->GetDirection())} |]
  v <- peek pvec 
  [C.exp| void { delete $(Vector3* pvec) } |]
  return v 

-- | Return up direction in parent space. Positive Y axis equals identity rotation.
nodeGetUp :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetUp p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->GetUp())} |]
  v <- peek pvec 
  [C.exp| void { delete $(Vector3* pvec) } |]
  return v 

-- | Return right direction in parent space. Positive X axis equals identity rotation.
nodeGetRight :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetRight p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->GetRight())} |]
  v <- peek pvec 
  [C.exp| void { delete $(Vector3* pvec) } |]
  return v 

-- | Return scale in parent space.
nodeGetScale :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetScale p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const Vector3* {&$(Node* ptr)->GetScale()} |]

-- | Return scale in parent space (for Urho2D).
nodeGetScale2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector2
nodeGetScale2D p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector2* {new Vector2($(Node* ptr)->GetScale2D())} |]
  v <- peek pvec 
  [C.exp| void {delete $(Vector2* pvec)} |]
  return v 

-- | Return parent space transform matrix.
nodeGetTransform :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Matrix3x4
nodeGetTransform p = liftIO $ do 
  let ptr = parentPointer p 
  pmat <- [C.exp| Matrix3x4* {new Matrix3x4($(Node* ptr)->GetTransform())} |]
  mat <- peek pmat 
  [C.exp| void { delete $(Matrix3x4* pmat) } |]
  return mat 

-- | Return position in world space.
nodeGetWorldPosition :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetWorldPosition p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->GetWorldPosition())} |]
  v <- peek pvec 
  [C.exp| void { delete $(Vector3* pvec) } |]
  return v 

-- | Return position in world space (for Urho2D).
nodeGetWorldPosition2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector2
nodeGetWorldPosition2D p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector2* {new Vector2($(Node* ptr)->GetWorldPosition2D())} |]
  v <- peek pvec 
  [C.exp| void {delete $(Vector2* pvec)} |]
  return v 

-- | Return rotation in world space.
nodeGetWorldRotation :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Quaternion
nodeGetWorldRotation p = liftIO $ do 
  let ptr = parentPointer p 
  pquat <- [C.exp| Quaternion* {new Quaternion($(Node* ptr)->GetWorldRotation())} |]
  quat <- peek pquat
  [C.exp| void { delete $(Quaternion* pquat) } |]
  return quat

-- | Return rotation in world space (for Urho2D).
nodeGetWorldRotation2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Float
nodeGetWorldRotation2D p = liftIO $ do 
  let ptr = parentPointer p 
  realToFrac <$> [C.exp| float {$(Node* ptr)->GetWorldRotation2D()} |]

-- | Return direction in world space.
nodeGetWorldDirection :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetWorldDirection p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->GetWorldDirection())} |]
  v <- peek pvec 
  [C.exp| void { delete $(Vector3* pvec) } |]
  return v 

-- | Return node's up vector in world space.
nodeGetWorldUp :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetWorldUp p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->GetWorldUp())} |]
  v <- peek pvec 
  [C.exp| void { delete $(Vector3* pvec) } |]
  return v 

-- | Return node's right vector in world space.
nodeGetWorldRight :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetWorldRight p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->GetWorldRight())} |]
  v <- peek pvec 
  [C.exp| void { delete $(Vector3* pvec) } |]
  return v 

-- | Return scale in world space.
nodeGetWorldScale :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector3
nodeGetWorldScale p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->GetWorldScale())} |]
  v <- peek pvec 
  [C.exp| void { delete $(Vector3* pvec) } |]
  return v 

-- | Return scale in world space (for Urho2D).
nodeGetWorldScale2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Vector2
nodeGetWorldScale2D p = liftIO $ do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector2* {new Vector2($(Node* ptr)->GetWorldScale2D())} |]
  v <- peek pvec 
  [C.exp| void {delete $(Vector2* pvec)} |]
  return v 

-- | Return world space transform matrix.
nodeGetWorldTransform :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Matrix3x4
nodeGetWorldTransform p = liftIO $ do 
  let ptr = parentPointer p 
  pmat <- [C.exp| Matrix3x4* {new Matrix3x4($(Node* ptr)->GetWorldTransform())} |]
  mat <- peek pmat 
  [C.exp| void { delete $(Matrix3x4* pmat) } |]
  return mat 

class NodeLocalToWorld a where 
  -- | Convert a local space position or rotation to world space.
  nodeLocalToWorld :: (Parent Node n, Pointer p n, MonadIO m)
    => p -- ^ Node pointer or pointer to ascentor
    -> a -- ^ position in local space
    -> m Vector3 -- ^ position in world space

instance NodeLocalToWorld Vector3 where 
  nodeLocalToWorld p v = liftIO $ with v $ \v' -> do 
    let ptr = parentPointer p 
    pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->LocalToWorld(*$(Vector3* v')))} |]
    vec <- peek pvec 
    [C.exp| void { delete $(Vector3* pvec) } |]
    return vec

instance NodeLocalToWorld Vector4 where 
  nodeLocalToWorld p v = liftIO $ with v $ \v' -> do 
    let ptr = parentPointer p 
    pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->LocalToWorld(*$(Vector4* v')))} |]
    vec <- peek pvec 
    [C.exp| void { delete $(Vector3* pvec) } |]
    return vec 

-- | Convert a local space position or rotation to world space (for Urho2D).
nodeLocalToWorld2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> Vector2
  -> m Vector2 
nodeLocalToWorld2D p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector2* {new Vector2($(Node* ptr)->LocalToWorld2D(*$(Vector2* v')))} |]
  vec <- peek pvec 
  [C.exp| void {delete $(Vector2* pvec)} |]
  return vec 

class NodeWorldToLocal a where 
  -- | Convert a world space position or rotation to local space.
  nodeWorldToLocal :: (Parent Node n, Pointer p n, MonadIO m)
    => p -- ^ Node pointer or pointer to ascentor
    -> a -- ^ position in local space
    -> m Vector3 -- ^ position in world space

instance NodeWorldToLocal Vector3 where 
  nodeWorldToLocal p v = liftIO $ with v $ \v' -> do 
    let ptr = parentPointer p 
    pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->WorldToLocal(*$(Vector3* v')))} |]
    vec <- peek pvec 
    [C.exp| void { delete $(Vector3* pvec) } |]
    return vec 

instance NodeWorldToLocal Vector4 where 
  nodeWorldToLocal p v = liftIO $ with v $ \v' -> do 
    let ptr = parentPointer p 
    pvec <- [C.exp| Vector3* {new Vector3($(Node* ptr)->WorldToLocal(*$(Vector4* v')))} |]
    vec <- peek pvec 
    [C.exp| void { delete $(Vector3* pvec) } |]
    return vec 

-- | Convert a world space position or rotation to local space (for Urho2D).
nodeWorldToLocal2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> Vector2
  -> m Vector2 
nodeWorldToLocal2D p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  pvec <- [C.exp| Vector2* {new Vector2($(Node* ptr)->WorldToLocal2D(*$(Vector2* v')))} |]
  vec <- peek pvec 
  [C.exp| void {delete $(Vector2* pvec)} |]
  return vec 

-- | Return whether transform has changed and world transform needs recalculation.
nodeIsDirty :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Bool
nodeIsDirty p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int)$(Node* ptr)->IsDirty()} |]

-- | Return number of child scene nodes.
nodeGetNumChildren :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> Bool -- ^ Recursive?
  -> m Int
nodeGetNumChildren p r = liftIO $ do 
  let ptr = parentPointer p 
      r' = fromBool r
  fromIntegral <$> [C.exp| unsigned int { $(Node* ptr)->GetNumChildren($(int r') != 0)} |]

-- | Return immediate child scene nodes.
nodeGetChildren  :: (Parent Node a, Pointer p a, MonadIO m, ForeignVectorRepresent v)
  => p -- ^ Node pointer or pointer to ascentor
  -> m (v SharedNodePtr)
nodeGetChildren  p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.exp| const VectorSharedNodePtr* { &$(Node* ptr)->GetChildren() } |]

-- | Return child scene nodes, optionally recursive.
nodeGetChildren' :: (Parent Node a, Pointer p a, MonadIO m, ForeignVectorRepresent v)
  => p -- ^ Node pointer or pointer to ascentor
  -> Bool -- ^ Recursive?
  -> m (v (Ptr Node))
nodeGetChildren' p r = liftIO $ withObject () $ \(pvec :: Ptr PODVectorNodePtr) -> do 
  let ptr = parentPointer p 
      r' = fromBool r
  [C.exp| void { $(Node* ptr)->GetChildren(*$(PODVectorNodePtr* pvec), $(int r') != 0)} |]
  peekForeignVectorAs' pvec

-- | Return child scene nodes with a specific component.
nodeGetChildrenWithComponent :: (Parent Node a, Pointer p a, MonadIO m, ForeignVectorRepresent v)
  => p -- ^ Node pointer or pointer to ascentor
  -> Ptr StringHash -- ^ type
  -> Bool -- ^ Recursive?
  -> m (v (Ptr Node))
nodeGetChildrenWithComponent p phash r = liftIO $ withObject () $ \(pvec :: Ptr PODVectorNodePtr) -> do 
  let ptr = parentPointer p 
      r' = fromBool r
  [C.exp| void { $(Node* ptr)->GetChildrenWithComponent(*$(PODVectorNodePtr* pvec), *$(StringHash* phash), $(int r') != 0)} |]
  peekForeignVectorAs' pvec

-- | Return child scene nodes with a specific tag.
nodeGetChildrenWithTag  :: (Parent Node a, Pointer p a, MonadIO m, ForeignVectorRepresent v)
  => p -- ^ Node pointer or pointer to ascentor
  -> String -- ^ tag 
  -> Bool -- ^ Recursive?
  -> m (v (Ptr Node))
nodeGetChildrenWithTag p tag r = liftIO $ withCString tag $ \tag' -> withObject () $ \(pvec :: Ptr PODVectorNodePtr) -> do 
  let ptr = parentPointer p 
      r' = fromBool r
  [C.exp| void { $(Node* ptr)->GetChildrenWithTag(*$(PODVectorNodePtr* pvec), String($(const char* tag')), $(int r') != 0) } |]
  peekForeignVectorAs' pvec

-- | Return child scene node by index.
nodeGetChildByIndex :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> Int -- ^ index
  -> m (Ptr Node)
nodeGetChildByIndex p i = liftIO $ do 
  let ptr = parentPointer p 
      i' = fromIntegral i
  [C.exp| Node* { $(Node* ptr)->GetChild($(unsigned int i'))} |]

-- | Return child scene node by name.
nodeGetChildByName :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> String -- ^ name 
  -> Bool -- ^ Recursive?
  -> m (Ptr Node)
nodeGetChildByName p n r = liftIO $ withCString n $ \n' -> do 
  let ptr = parentPointer p 
      r' = fromBool r
  [C.exp| Node* { $(Node* ptr)->GetChild($(const char* n'), $(int r') != 0)} |]

-- | Return child scene node by name hash.
nodeGetChildByNameHash :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> Ptr StringHash -- ^ name hash
  -> Bool -- ^ Recursive?
  -> m (Ptr Node)
nodeGetChildByNameHash p phash r = liftIO $ do 
  let ptr = parentPointer p 
      r' = fromBool r
  [C.exp| Node* { $(Node* ptr)->GetChild(*$(StringHash* phash), $(int r') != 0)} |]

-- | Return number of components.
nodeGetNumComponents :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Int
nodeGetNumComponents p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| unsigned int { $(Node* ptr)->GetNumComponents()} |]

-- | Return number of non-local components.
nodeGetNumNetworkComponents :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m Int
nodeGetNumNetworkComponents p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| unsigned int { $(Node* ptr)->GetNumNetworkComponents()} |]

-- | Return all components.
nodeGetComponents :: (Parent Node a, Pointer p a, MonadIO m, ForeignVectorRepresent v)
  => p -- ^ Node pointer or pointer to ascentor
  -> m (v SharedComponentPtr)
nodeGetComponents p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.exp| const VectorSharedComponentPtr* { &$(Node* ptr)->GetComponents()} |]

-- | Return all components of type. Optionally recursive.
nodeGetComponentsByType  :: (Parent Node a, Pointer p a, MonadIO m, ForeignVectorRepresent v)
  => p -- ^ Node pointer or pointer to ascentor
  -> Ptr StringHash -- ^ type
  -> Bool -- ^ Recursive?
  -> m (v (Ptr Component))
nodeGetComponentsByType p phash r = liftIO $ withObject () $ \(pvec :: Ptr PODVectorComponentPtr) -> do 
  let ptr = parentPointer p 
      r' = fromBool r
  [C.exp| void { $(Node* ptr)->GetComponents(*$(PODVectorComponentPtr* pvec), *$(StringHash* phash), $(int r') != 0) } |]
  peekForeignVectorAs' pvec 

-- | Return component by type. If there are several, returns the first. High-level version of 'nodeGetComponent'
nodeGetComponent' :: forall a p c m . (Parent Node a, Pointer p a, MonadIO m, NodeComponent c) 
  => p -- ^ Node pointer or pointer to ascentor
  -> Bool -- ^ Recursive?
  -> m (Maybe (Ptr c))
nodeGetComponent' p r = do
  let ct = nodeComponentType (Proxy :: Proxy c)
  cp <- nodeGetComponent p ct r
  join <$> checkNullPtr' cp (return . castToChild)

-- | Return component by type. If there are several, returns the first.
nodeGetComponent :: (Parent Node a, Pointer p a, MonadIO m) 
  => p -- ^ Node pointer or pointer to ascentor
  -> Ptr StringHash -- ^ type
  -> Bool -- ^ Recursive?
  -> m (Ptr Component)
nodeGetComponent p ct r = liftIO $ do 
  let ptr = parentPointer p
      r' = fromBool r
  [C.exp| Component* { $(Node* ptr)->GetComponent(*$(StringHash* ct), $(int r') != 0) } |]

-- | Return component in parent node. If there are several, returns the first. May optional traverse up to the root node.
nodeGetParentComponent :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> Ptr StringHash -- ^ type 
  -> Bool -- ^ full traversal
  -> m (Ptr Component)
nodeGetParentComponent p phash r = liftIO $ do 
  let ptr = parentPointer p 
      r' = fromBool r
  [C.exp| Component* { $(Node* ptr)->GetParentComponent(*$(StringHash* phash), $(int r') != 0)} |]

-- | Return whether has a specific component.
nodeHasComponent :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> Ptr StringHash -- ^ type
  -> m Bool
nodeHasComponent p phash = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int)$(Node* ptr)->HasComponent(*$(StringHash* phash))} |]

-- | Return listener components.
nodeGetListeners :: (Parent Node a, Pointer p a, MonadIO m, ForeignVectorRepresent v)
  => p -- ^ Node pointer or pointer to ascentor
  -> m (v SharedWeakComponentPtr)
nodeGetListeners p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.block| const VectorSharedWeakComponentPtr* { 
      static Vector<WeakPtr<Component> > vec = $(Node* ptr)->GetListeners();
      return &vec;
    } |]

-- | Return a user variable.
nodeGetVar :: (Parent Node a, Pointer p a, MonadIO m, VariantStorable b)
  => p -- ^ Node pointer or pointer to ascentor
  -> String -- ^ key
  -> m (Maybe b)
nodeGetVar p key = liftIO $ withObject key $ \pkey -> do 
  let ptr = parentPointer p 
  getVariant =<< [C.exp| const Variant* { &$(Node* ptr)->GetVar(*$(StringHash* pkey))} |]

-- | Return all user variables.
nodeGetVars :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or pointer to ascentor
  -> m (Ptr VariantMap)
nodeGetVars p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| const HashMapStringHashVariant* { &$(Node* ptr)->GetVars()} |]

-- Stopped at: https://github.com/urho3d/Urho3D/blob/master/Source/Urho3D/Scene/Node.h#L520