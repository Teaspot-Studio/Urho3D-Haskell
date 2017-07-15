{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.DecalSet(
    DecalSet
  , DecalVertex(..)
  , PODVectorDecalVertex
  , HasPosition(..)
  , HasNormal(..)
  , HasTexCoord(..)
  , HasTangent(..)
  , HasBlendWeights0(..)
  , HasBlendWeights1(..)
  , HasBlendWeights2(..)
  , HasBlendWeights3(..)
  , HasBlendIndices0(..)
  , HasBlendIndices1(..)
  , HasBlendIndices2(..)
  , HasBlendIndices3(..)
  , Decal(..)
  , HasTimer(..)
  , HasTimeToLive(..)
  , HasBoundingBox(..)
  , HasVertecies(..)
  , HasIndices(..)
  , decalSetContext
  , decalSetSetMaterial
  , decalSetSetMaxVertices
  , decalSetSetMaxIndices
  , decalSetSetOptimizeBufferSize
  , defaultSubGeometry
  , decalSetAddDecal
  , decalSetRemoveDecals
  , decalSetRemoveAllDecals
  , decalSetGetMaterial
  , decalSetGetNumDecals
  , decalSetGetNumVertices
  , decalSetGetNumIndices
  , decalSetGetMaxVertices
  , decalSetGetMaxIndices
  , decalSetGetOptimizeBufferSize
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Graphics.Internal.DecalSet
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Scene.Node
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ

import Graphics.Urho3D.Core.Object
--import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Graphics.Drawable
import Graphics.Urho3D.Graphics.Material
import Graphics.Urho3D.Math.Quaternion
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Math.Vector4
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx
  <> decalSetCntx
  <> componentContext
  <> animatableContext
  <> serializableContext
  <> drawableContext
  <> objectContext
  <> vector3Context
  <> vector2Context
  <> vector4Context
  <> materialContext
  <> quaternionContext
  )
C.include "<Urho3D/Graphics/DecalSet.h>"
C.using "namespace Urho3D"

C.verbatim "typedef PODVector<DecalVertex> PODVectorDecalVertex;"

simplePODVector "DecalVertex"

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

decalSetContext :: C.Context
decalSetContext = componentContext <> decalSetCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Drawable] ''DecalSet

instance NodeComponent DecalSet where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { DecalSet::GetTypeStatic().Value() } |]

instance Storable DecalVertex where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(DecalVertex) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<DecalVertex>::AlignmentOf } |]
  peek ptr = do
      _decalVertexPosition <- peek =<< [C.exp| Vector3* { &$(DecalVertex* ptr)->position_ } |]
      _decalVertexNormal <- peek =<< [C.exp| Vector3* { &$(DecalVertex* ptr)->normal_ } |]
      _decalVertexTexCoord <- peek =<< [C.exp| Vector2* { &$(DecalVertex* ptr)->texCoord_ } |]
      _decalVertexTangent <- peek =<< [C.exp| Vector4* { &$(DecalVertex* ptr)->tangent_ } |]
      _decalVertexBlendWeights0 <- realToFrac <$> [C.exp| float { $(DecalVertex* ptr)->blendWeights_[0] } |]
      _decalVertexBlendWeights1 <- realToFrac <$> [C.exp| float { $(DecalVertex* ptr)->blendWeights_[1] } |]
      _decalVertexBlendWeights2 <- realToFrac <$> [C.exp| float { $(DecalVertex* ptr)->blendWeights_[2] } |]
      _decalVertexBlendWeights3 <- realToFrac <$> [C.exp| float { $(DecalVertex* ptr)->blendWeights_[3] } |]
      _decalVertexBlendIndices0 <- fromIntegral <$> [C.exp| unsigned char { $(DecalVertex* ptr)->blendIndices_[0] } |]
      _decalVertexBlendIndices1 <- fromIntegral <$> [C.exp| unsigned char { $(DecalVertex* ptr)->blendIndices_[1] } |]
      _decalVertexBlendIndices2 <- fromIntegral <$> [C.exp| unsigned char { $(DecalVertex* ptr)->blendIndices_[2] } |]
      _decalVertexBlendIndices3 <- fromIntegral <$> [C.exp| unsigned char { $(DecalVertex* ptr)->blendIndices_[3] } |]
      return DecalVertex {..}
  poke ptr DecalVertex {..} =
    with _decalVertexPosition $ \_decalVertexPosition' ->
    with _decalVertexPosition $ \_decalVertexPosition' ->
    with _decalVertexNormal $ \_decalVertexNormal' ->
    with _decalVertexTexCoord $ \_decalVertexTexCoord' ->
    with _decalVertexTangent $ \_decalVertexTangent' ->
    [C.block| void {
      $(DecalVertex* ptr)->position_ = *$(Vector3* _decalVertexPosition');
      $(DecalVertex* ptr)->normal_ = *$(Vector3* _decalVertexNormal');
      $(DecalVertex* ptr)->texCoord_ = *$(Vector2* _decalVertexTexCoord');
      $(DecalVertex* ptr)->tangent_ = *$(Vector4* _decalVertexTangent');
      $(DecalVertex* ptr)->blendWeights_[0] = $(float _decalVertexBlendWeights0');
      $(DecalVertex* ptr)->blendWeights_[1] = $(float _decalVertexBlendWeights1');
      $(DecalVertex* ptr)->blendWeights_[2] = $(float _decalVertexBlendWeights2');
      $(DecalVertex* ptr)->blendWeights_[3] = $(float _decalVertexBlendWeights3');
      $(DecalVertex* ptr)->blendIndices_[0] = $(unsigned char _decalVertexBlendIndices0');
      $(DecalVertex* ptr)->blendIndices_[1] = $(unsigned char _decalVertexBlendIndices1');
      $(DecalVertex* ptr)->blendIndices_[2] = $(unsigned char _decalVertexBlendIndices2');
      $(DecalVertex* ptr)->blendIndices_[3] = $(unsigned char _decalVertexBlendIndices3');
     } |]
    where
    _decalVertexBlendWeights0' = realToFrac _decalVertexBlendWeights0
    _decalVertexBlendWeights1' = realToFrac _decalVertexBlendWeights1
    _decalVertexBlendWeights2' = realToFrac _decalVertexBlendWeights2
    _decalVertexBlendWeights3' = realToFrac _decalVertexBlendWeights3
    _decalVertexBlendIndices0' = fromIntegral _decalVertexBlendIndices0
    _decalVertexBlendIndices1' = fromIntegral _decalVertexBlendIndices1
    _decalVertexBlendIndices2' = fromIntegral _decalVertexBlendIndices2
    _decalVertexBlendIndices3' = fromIntegral _decalVertexBlendIndices3


-- -- | Return vertical field of view in degrees.
-- decalSetGetFov :: (Parent DecalSet a, Pointer p a, MonadIO m) => p -- ^ DecalSet pointer or child
--   -> m Float
-- decalSetGetFov p = liftIO $ do
--   let ptr = parentPointer p
--   realToFrac <$> [C.exp| float {$(DecalSet* ptr)->GetFov()} |]

-- | Set material. The material should use a small negative depth bias to avoid Z-fighting.
-- void SetMaterial(Material* material);
decalSetSetMaterial :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> Ptr Material
  -> m ()
decalSetSetMaterial p mat = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(DecalSet* ptr)->SetMaterial($(Material* mat))} |]

-- | Set maximum number of decal vertices.
-- void SetMaxVertices(unsigned num);
decalSetSetMaxVertices :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> Word
  -> m ()
decalSetSetMaxVertices p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void {$(DecalSet* ptr)->SetMaxVertices($(unsigned int v'))} |]

-- | Set maximum number of decal vertex indices.
-- void SetMaxIndices(unsigned num);
decalSetSetMaxIndices :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> Word
  -> m ()
decalSetSetMaxIndices p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void {$(DecalSet* ptr)->SetMaxIndices($(unsigned int v'))} |]

-- | Set whether to optimize GPU buffer sizes according to current amount of decals. Default false, which will size the buffers according to the maximum vertices/indices. When true, buffers will be reallocated whenever decals are added/removed, which can be worse for performance.
-- void SetOptimizeBufferSize(bool enable);
decalSetSetOptimizeBufferSize :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> Bool
  -> m ()
decalSetSetOptimizeBufferSize p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(DecalSet* ptr)->SetOptimizeBufferSize($(int v') != 0)} |]

-- | Default value for 'decalSetAddDecal' sub geometry argument
defaultSubGeometry :: Word
defaultSubGeometry = fromIntegral [C.pure| unsigned int { M_MAX_UNSIGNED } |]

-- | Add a decal at world coordinates, using a target drawable's geometry for reference. If the decal needs to move with the target, the decal component should be created to the target's node. Return true if successful.
-- bool AddDecal(Drawable* target, const Vector3& worldPosition, const Quaternion& worldRotation, float size, float aspectRatio,
--    float depth, const Vector2& topLeftUV, const Vector2& bottomRightUV, float timeToLive = 0.0f, float normalCutoff = 0.1f,
--    unsigned subGeometry = M_MAX_UNSIGNED);
decalSetAddDecal :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> Ptr Drawable -- ^ target
  -> Vector3 -- ^ world position
  -> Quaternion -- ^ world rotation
  -> Float -- ^ size
  -> Float -- ^ aspect ration
  -> Float -- ^ depth
  -> Vector2 -- ^ topLeftUV
  -> Vector2 -- ^ bottomRightUV
  -> Float -- ^ timeToLive (default 0)
  -> Float -- ^ normalCutOff (default 0.1)
  -> Word -- ^ sub geometry (default defaultSubGeometry)
  -> m Bool
decalSetAddDecal p target worldPosition worldRotation size aspectRatio depth topLeftUV bottomRightUV timeToLiveArg normalCutOff subGeometry = liftIO $
  with worldPosition $ \worldPosition' ->
  with worldRotation $ \worldRotation' ->
  with topLeftUV $ \topLeftUV' ->
  with bottomRightUV $ \bottomRightUV' -> do
  let ptr = parentPointer p
      size' = realToFrac size
      aspectRatio' = realToFrac aspectRatio
      depth' = realToFrac depth
      timeToLive' = realToFrac timeToLiveArg
      normalCutOff' = realToFrac normalCutOff
      subGeometry' = fromIntegral subGeometry
  toBool <$> [C.exp| int {(int)$(DecalSet* ptr)->AddDecal(
      $(Drawable* target)
    , *$(Vector3* worldPosition')
    , *$(Quaternion* worldRotation')
    , $(float size')
    , $(float aspectRatio')
    , $(float depth')
    , *$(Vector2* topLeftUV')
    , *$(Vector2* bottomRightUV')
    , $(float timeToLive')
    , $(float normalCutOff')
    , $(unsigned int subGeometry'))
    } |]

-- | Remove n oldest decals.
-- void RemoveDecals(unsigned num);
decalSetRemoveDecals :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> Word
  -> m ()
decalSetRemoveDecals p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void {$(DecalSet* ptr)->RemoveDecals($(unsigned int v'))} |]

-- | Remove all decals.
-- void RemoveAllDecals();
decalSetRemoveAllDecals :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> m ()
decalSetRemoveAllDecals p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(DecalSet* ptr)->RemoveAllDecals()} |]

-- | Return material.
-- Material* GetMaterial() const;
decalSetGetMaterial :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> m (Ptr Material)
decalSetGetMaterial p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Material* {$(DecalSet* ptr)->GetMaterial()} |]

-- | Return number of decals.
-- unsigned GetNumDecals() const { return decals_.Size(); }
decalSetGetNumDecals :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> m Word
decalSetGetNumDecals p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(DecalSet* ptr)->GetNumDecals()} |]

-- | Retur number of vertices in the decals.
-- unsigned GetNumVertices() const { return numVertices_; }
decalSetGetNumVertices :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> m Word
decalSetGetNumVertices p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(DecalSet* ptr)->GetNumVertices()} |]

-- | Retur number of vertex indices in the decals.
-- unsigned GetNumIndices() const { return numIndices_; }
decalSetGetNumIndices :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> m Word
decalSetGetNumIndices p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(DecalSet* ptr)->GetNumIndices()} |]

-- | Return maximum number of decal vertices.
-- unsigned GetMaxVertices() const { return maxVertices_; }
decalSetGetMaxVertices :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> m Word
decalSetGetMaxVertices p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(DecalSet* ptr)->GetMaxVertices()} |]

-- | Return maximum number of decal vertex indices.
-- unsigned GetMaxIndices() const { return maxIndices_; }
decalSetGetMaxIndices :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> m Word
decalSetGetMaxIndices p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(DecalSet* ptr)->GetMaxIndices()} |]

-- | Return whether is optimizing GPU buffer sizes according to current amount of decals.
-- bool GetOptimizeBufferSize() const { return optimizeBufferSize_; }
decalSetGetOptimizeBufferSize :: (Parent DecalSet a, Pointer p a, MonadIO m)
  => p -- ^ DecalSet pointer or child
  -> m Bool
decalSetGetOptimizeBufferSize p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(DecalSet* ptr)->GetOptimizeBufferSize()} |]
