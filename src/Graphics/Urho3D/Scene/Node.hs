{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  -- | Getters
  , nodeGetComponent
  , nodeGetRotation
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Data.Proxy
import Foreign 
import Foreign.C.String
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.Quaternion
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Internal.Node
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> nodeCntx <> sharedNodePtrCntx <> contextContext <> stringHashContext <> componentContext <> quaternionContext <> vector2Context <> vector3Context)
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
  [C.exp| void { $(Node* ptr)->Rotate($(float q'), (TransformSpace)$(int s')) } |]

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
  [C.exp| void { $(Node* ptr)->RotateAround(*$(Vector2* pv'), $(float q'), (TransformSpace)$(int s')) } |]


nodeGetComponent :: forall a p c m . (Parent Node a, Pointer p a, MonadIO m, NodeComponent c) 
  => p -> m (Maybe (Ptr c))
nodeGetComponent p = liftIO $ do 
  let ptr = parentPointer p 
      ct = nodeComponentType (Proxy :: Proxy c)
  cp <- [C.exp| Component* { $(Node* ptr)->GetComponent(*$(StringHash* ct)) } |]
  join <$> checkNullPtr' cp (return . castToChild)

-- | Returns node rotation in quaternion
nodeGetRotation :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> m Quaternion
nodeGetRotation p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const Quaternion* { &$(Node* ptr)->GetRotation() } |]
  
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

-- https://github.com/urho3d/Urho3D/blob/master/Source/Urho3D/Scene/Node.h#L234