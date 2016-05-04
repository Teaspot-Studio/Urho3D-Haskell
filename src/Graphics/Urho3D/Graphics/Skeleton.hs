{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Skeleton(
    Skeleton 
  , Bone(..)
  , HasName(..)
  , HasParentIndex(..)
  , HasInitialPosition(..)
  , HasInitialRotation(..)
  , HasInitialScale(..)
  , HasOffsetMatrix(..)
  , HasAnimated(..)
  , HasCollisionMask(..)
  , HasRadius(..)
  , HasBoundingBox(..)
  , HasNode(..)
  , skeletonContext 
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import Foreign.C.String
import Text.RawString.QQ

import Graphics.Urho3D.Graphics.Internal.Skeleton

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Math.Quaternion
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Scene.Node 

C.context (C.cppCtx 
  <> skeletonCntx 
  <> contextContext
  <> vector3Context
  <> quaternionContext
  <> matrix3x4Context
  <> boundingBoxContext
  <> nodeContext)

C.include "<Urho3D/Graphics/Skeleton.h>"
C.using "namespace Urho3D"

C.verbatim "typedef WeakPtr<Node> WeakNode;"

skeletonContext :: C.Context 
skeletonContext = skeletonCntx

instance Createable (Ptr Skeleton) where 
  type CreationOptions (Ptr Skeleton) = ()

  newObject _ = liftIO $ [C.exp| Skeleton* { new Skeleton() } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Skeleton* ptr) } |]

C.verbatim [r|
template <class T>
class Traits
{
public:
    struct AlignmentFinder
    {
      char a; 
      T b;
    };

    enum {AlignmentOf = sizeof(AlignmentFinder) - sizeof(T)};
};
|]

instance Storable Bone where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Bone) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Bone>::AlignmentOf } |]
  peek ptr = do 
    _boneName <- peekCString =<< [C.exp| const char* { $(Bone* ptr)->name_.CString() } |]
    _boneParentIndex <- fromIntegral <$> [C.exp| unsigned int { $(Bone* ptr)->parentIndex_} |]
    _boneInitialPosition <- peek =<< [C.exp| Vector3* { &$(Bone* ptr)->initialPosition_} |]
    _boneInitialRotation <- peek =<< [C.exp| Quaternion* { &$(Bone* ptr)->initialRotation_} |]
    _boneInitialScale <- peek =<< [C.exp| Vector3* { &$(Bone* ptr)->initialScale_} |]
    _boneOffsetMatrix <- peek =<< [C.exp| Matrix3x4* { &$(Bone* ptr)->offsetMatrix_} |]
    _boneAnimated <- toBool <$> [C.exp| int { (int)$(Bone* ptr)->animated_} |]
    _boneCollisionMask <- fromIntegral <$> [C.exp| unsigned char { $(Bone* ptr)->collisionMask_} |]
    _boneRadius <- realToFrac <$> [C.exp| float { $(Bone* ptr)->radius_} |]
    _boneBoundingBox <- peek =<< [C.exp| BoundingBox* { &$(Bone* ptr)->boundingBox_} |]
    _boneNode <- peekWeakPtr =<< [C.exp| WeakNode* { new WeakPtr<Node>($(Bone* ptr)->node_)} |]
    return $ Bone {..}

  poke ptr Bone{..} = 
    withCString _boneName $ \_boneName' -> 
    withObject _boneName $ \_boneNameHash' ->
    with _boneInitialPosition $ \_boneInitialPosition' ->
    with _boneInitialRotation $ \_boneInitialRotation' ->
    with _boneInitialScale $ \_boneInitialScale' ->
    with _boneOffsetMatrix $ \_boneOffsetMatrix' ->
    with _boneBoundingBox $ \_boneBoundingBox' -> do 
      let _boneParentIndex' = fromIntegral _boneParentIndex
          _boneAnimated' = fromBool _boneAnimated
          _boneRadius' = realToFrac _boneRadius
          _boneNode' = parentPointer _boneNode
          _boneCollisionMask' = fromIntegral _boneCollisionMask
      [C.block| void { 
        $(Bone* ptr)->name_ = String($(const char* _boneName'));
        $(Bone* ptr)->nameHash_ = *$(StringHash* _boneNameHash');
        $(Bone* ptr)->parentIndex_ = $(unsigned int _boneParentIndex');
        $(Bone* ptr)->initialPosition_ = *$(Vector3* _boneInitialPosition');
        $(Bone* ptr)->initialRotation_ = *$(Quaternion* _boneInitialRotation');
        $(Bone* ptr)->initialScale_ = *$(Vector3* _boneInitialScale');
        $(Bone* ptr)->offsetMatrix_ = *$(Matrix3x4* _boneOffsetMatrix');
        $(Bone* ptr)->animated_ = $(int _boneAnimated') != 0;
        $(Bone* ptr)->collisionMask_ = $(unsigned char _boneCollisionMask');
        $(Bone* ptr)->radius_ = $(float _boneRadius');
        $(Bone* ptr)->boundingBox_ = *$(BoundingBox* _boneBoundingBox');
        $(Bone* ptr)->node_ = WeakPtr<Node>($(Node* _boneNode'));
      } |]