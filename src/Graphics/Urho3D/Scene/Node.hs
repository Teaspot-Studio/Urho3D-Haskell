{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Node(
    Node
  , nodeContext
  , SharedNode
  , SharedNodePtr
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
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.Quaternion
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Internal.Scene
import Graphics.Urho3D.Scene.Internal.Node
import Graphics.Urho3D.Network.Connection
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> nodeCntx <> sharedNodePtrCntx <> contextContext <> stringHashContext <> componentContext <> quaternionContext <> vector2Context <> vector3Context <> connectionContext <> variantContext <> stringContext <> sceneCntx <> matrix3x4Context)
C.include "<Urho3D/Scene/Node.h>"
C.include "<Urho3D/Scene/Component.h>"
C.using "namespace Urho3D" 

nodeContext :: C.Context 
nodeContext = sharedNodePtrCntx <> nodeCntx <> stringHashContext <> componentContext

newNode :: Ptr Context -> IO (Ptr Node)
newNode ptr = [C.exp| Node* { new Node($(Context* ptr)) } |]

deleteNode :: Ptr Node -> IO ()
deleteNode ptr = [C.exp| void { delete $(Node* ptr) } |]

instance Createable (Ptr Node) where 
  type CreationOptions (Ptr Node) = Ptr Context 

  newObject = liftIO . newNode
  deleteObject = liftIO . deleteNode

sharedPtr "Node" 

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


nodeGetComponent :: forall a p c m . (Parent Node a, Pointer p a, MonadIO m, NodeComponent c) 
  => p -> m (Maybe (Ptr c))
nodeGetComponent p = liftIO $ do 
  let ptr = parentPointer p 
      ct = nodeComponentType (Proxy :: Proxy c)
  cp <- [C.exp| Component* { $(Node* ptr)->GetComponent(*$(StringHash* ct)) } |]
  join <$> checkNullPtr' cp (return . castToChild)
  
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
  => p -- ^ Node pointer or children
  -> CreateMode 
  -> m (Ptr Node)
nodeClone p cm = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Node* {$(Node* ptr)->Clone()} |]

-- | Remove from the parent node. If no other shared pointer references exist, causes immediate deletion.
nodeRemove:: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m ()
nodeRemove p = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| void {$(Node* ptr)->Remove()} |]

-- | Set parent scene node. Retains the world transform.
nodeSetParent:: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> Ptr Node -- ^ parent
  -> m ()
nodeSetParent p parent = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| void {$(Node* ptr)->SetParent()} |]

-- | Set a user variable.
nodeSetVar :: (Parent Node a, Pointer p a, VariantStorable v, MonadIO m)
  => p -- ^ Node pointer or children
  -> String -- ^ key
  -> v -- ^ value
  -> m ()
nodeSetVar p key value = liftIO $ withObject key $ \pkey -> withVariant value $ \pvalue -> do 
  let ptr = parentPointer p
  [C.exp| void {$(Node* ptr)->SetVar(*$(StringHash* pkey), *$(Variant* pvalue))} |]

-- | Add listener component that is notified of node being dirtied. Can either be in the same node or another.
nodeAddListener:: (Parent Node a, Pointer p a, Parent Component b, Pointer pComponent b, MonadIO m)
  => p -- ^ Node pointer or children
  -> pComponent -- ^ Pointer to component
  -> m ()
nodeAddListener p pcmp = liftIO $ do 
  let ptr = parentPointer p
      cmp = parentPointer pcmp
  [C.exp| void {$(Node* ptr)->AddListener($(Component* cmp))} |]

-- | Remove listener component.
nodeRemoveListener:: (Parent Node a, Pointer p a, Parent Component b, Pointer pComponent b, MonadIO m)
  => p -- ^ Node pointer or children
  -> pComponent -- ^ Pointer to component
  -> m ()
nodeRemoveListener p pcmp = liftIO $ do 
  let ptr = parentPointer p
      cmp = parentPointer pcmp
  [C.exp| void {$(Node* ptr)->($(Component* cmp))} |]

-- | Return ID.
nodeGetID :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Word
nodeGetID p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| unsigned int {$(Node* ptr)->GetID()} |]

-- | Return name.
nodeGetName :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m String
nodeGetName p = liftIO $ do 
  let ptr = parentPointer p 
  loadConstUrhoString =<< [C.exp| const String* {&$(Node* ptr)->GetName()} |]

-- | Return name hash. You need to delete the hash.
nodeGetNameHash :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m (Ptr StringHash)
nodeGetNameHash p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| StringHash* {new StringHash($(Node* ptr)->GetNameHash())} |]

-- | Return all tags.
nodeGetTags :: (Parent Node a, Pointer p a, MonadIO m, ForeignVectorRepresent v)
  => p -- ^ Node pointer or children
  -> m (v String) -- ^ Container with tags
nodeGetTags p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.exp| StringVector* {&$(Node* ptr)->GetTags()} |]

-- | Return whether has a specific tag.
nodeHasTag :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> String -- ^ tag
  -> m Bool
nodeHasTag p tag = liftIO $ withCString tag $ \tag' -> do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int {(int)$(Node* ptr)->HasTag(String($(const char* tag')))} |]

-- | Return parent scene node.
nodeGetParent :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m (Ptr Node)
nodeGetParent p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Node* {$(Node* ptr)->GetParent()} |]

-- | Return scene.
nodeGetScene :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m (Ptr Scene)
nodeGetScene p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Scene* {$(Node* ptr)->GetScene()} |]

-- | Return whether is enabled. Disables nodes effectively disable all their components.
nodeIsEnabled :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Bool
nodeIsEnabled p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int {(int)$(Node* ptr)->IsEnabled()} |]

-- | Returns the node's last own enabled state. May be different than the value returned by IsEnabled when SetDeepEnabled has been used.
nodeIsEnabledSelf :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Bool
nodeIsEnabledSelf p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int {(int)$(Node* ptr)->IsEnabledSelf()} |]

-- | Return owner connection in networking.
nodeGetOwner :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m (Ptr Connection)
nodeGetOwner p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Connection* {$(Node* ptr)->GetOwner()} |]

-- | Return position in parent space.
nodeGetPosition :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetPosition p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {&$(Node* ptr)->GetPosition()} |]

-- | Return position in parent space (for Urho2D).
nodeGetPosition2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector2
nodeGetPosition2D p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector2* {&$(Node* ptr)->GetPosition2D()} |]

-- | Returns node rotation in quaternion
nodeGetRotation :: (Parent Node a, Pointer p a, MonadIO m) 
  => p -- ^ Node pointer or child
  -> m Quaternion
nodeGetRotation p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const Quaternion* { &$(Node* ptr)->GetRotation() } |]

-- | Return rotation in parent space (for Urho2D).
nodeGetRotation2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Float
nodeGetRotation2D p = liftIO $ do 
  let ptr = parentPointer p 
  realToFrac <$> [C.exp| float {$(Node* ptr)->GetRotation2D()} |]

-- | Return forward direction in parent space. Positive Z axis equals identity rotation.
nodeGetDirection :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetDirection p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {&$(Node* ptr)->GetDirection()} |]

-- | Return up direction in parent space. Positive Y axis equals identity rotation.
nodeGetUp :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetUp p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {&$(Node* ptr)->GetUp()} |]

-- | Return right direction in parent space. Positive X axis equals identity rotation.
nodeGetRight :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetRight p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {$(Node* ptr)->GetRight()} |]

-- | Return scale in parent space.
nodeGetScale :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetScale p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {&$(Node* ptr)->GetScale()} |]

-- | Return scale in parent space (for Urho2D).
nodeGetScale2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector2
nodeGetScale2D p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector2* {&$(Node* ptr)->GetScale2D()} |]

-- | Return parent space transform matrix.
nodeGetTransform :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Matrix3x4
nodeGetTransform p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Matrix3x4* {&$(Node* ptr)->GetTransform()} |]

-- | Return position in world space.
nodeGetWorldPosition :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetWorldPosition p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {&$(Node* ptr)->GetWorldPosition()} |]

-- | Return position in world space (for Urho2D).
nodeGetWorldPosition2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector2
nodeGetWorldPosition2D p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector2* {&$(Node* ptr)->GetWorldPosition2D()} |]

-- | Return rotation in world space.
nodeGetWorldRotation :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Quaternion
nodeGetWorldRotation p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Quaternion* {&$(Node* ptr)->GetWorldRotation()} |]

-- | Return rotation in world space (for Urho2D).
nodeGetWorldRotation2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Float
nodeGetWorldRotation2D p = liftIO $ do 
  let ptr = parentPointer p 
  realToFrac <$> [C.exp| float {$(Node* ptr)->GetWorldRotation2D()} |]

-- | Return direction in world space.
nodeGetWorldDirection :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetWorldDirection p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {&$(Node* ptr)->GetWorldDirection()} |]

-- | Return node's up vector in world space.
nodeGetWorldUp :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetWorldUp p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {&$(Node* ptr)->GetWorldUp()} |]

-- | Return node's right vector in world space.
nodeGetWorldRight :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetWorldRight p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {&$(Node* ptr)->GetWorldRight()} |]

-- | Return scale in world space.
nodeGetWorldScale :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector3
nodeGetWorldScale p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector3* {&$(Node* ptr)->GetWorldScale()} |]

-- | Return scale in world space (for Urho2D).
nodeGetWorldScale2D :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Vector2
nodeGetWorldScale2D p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Vector2* {&$(Node* ptr)->GetWorldScale2D()} |]

-- | Return world space transform matrix.
nodeGetWorldTransform :: (Parent Node a, Pointer p a, MonadIO m)
  => p -- ^ Node pointer or children
  -> m Matrix3x4
nodeGetWorldTransform p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| Matrix3x4* {&$(Node* ptr)->GetWorldTransform()} |]

-- Stopped at: https://github.com/urho3d/Urho3D/blob/master/Source/Urho3D/Scene/Node.h#L274