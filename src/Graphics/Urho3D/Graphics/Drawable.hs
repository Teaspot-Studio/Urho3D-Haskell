{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.Drawable(
    Drawable
  , PODVectorDrawablePtr
  , VectorSourceBatch
  , SourceBatch(..)
  , HasDistance(..)
  , HasGeometry(..)
  , HasMaterial(..)
  , HasWorldTransform(..)
  , HasNumWorldTransforms(..)
  , HasGeometryType(..)
  , FrameInfo(..)
  , HasFrameNumber(..)
  , HasTimeStep(..)
  , HasViewSize(..)
  , HasCamera(..)
  , drawableContext
  , drawableSetDrawDistance
  , drawableSetShadowDistance
  , drawableSetLodBias
  , drawableSetViewMask
  , drawableSetLightMask
  , drawableSetShadowMask
  , drawableSetZoneMask
  , drawableSetMaxLights
  , drawableSetCastShadows
  , drawableSetOccluder
  , drawableSetOccludee
  , drawableMarkForUpdate
  , drawableGetBoundingBox
  , drawableGetWorldBoundingBox
  , drawableGetDrawableFlags
  , drawableGetDrawDistance
  , drawableGetShadowDistance
  , drawableGetLodBias
  , drawableGetViewMask
  , drawableGetLightMask
  , drawableGetShadowMask
  , drawableGetZoneMask
  , drawableGetMaxLights
  , drawableGetCastShadows
  , drawableIsOccluder
  , drawableIsOccludee
  , drawableIsInView
  , drawableIsInViewCam
  , drawableGetBatches
  , drawableSetZone
  , drawableSetSortValue
  , drawableSetMinMaxZ
  , drawableMarkInView
  , drawableMarkInView'
  , drawableLimitLights
  , drawableLimitVertexLights
  , drawableSetBasePass
  , drawableGetOctant
  , drawableGetZone
  , drawableIsZoneDirty
  , drawableGetDistance
  , drawableGetLodDistance
  , drawableGetSortValue
  , drawableIsInViewFrame
  , drawableHasBasePass
  , drawableGetLights
  , drawableGetVertexLights
  , drawableGetFirstLight
  , drawableGetMinZ
  , drawableGetMaxZ
  , drawableAddLight
  , drawableAddVertexLight
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Drawable
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import Foreign
import Text.RawString.QQ

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Scene.Node
import Graphics.Urho3D.Container.ForeignVector

import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Camera
import Graphics.Urho3D.Graphics.Geometry
import Graphics.Urho3D.Graphics.Light
import Graphics.Urho3D.Graphics.Material
import Graphics.Urho3D.Graphics.Octree
import Graphics.Urho3D.Graphics.Zone
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx
  <> drawableCntx
  <> componentContext
  <> stringHashContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> boundingBoxContext
  <> zoneContext
  <> octreeContext
  <> lightContext
  <> cameraContext
  <> vector2Context
  <> geometryContext
  <> materialContext
  <> matrix3x4Context
  <> vectorSourceBatchCntx)

C.include "<Urho3D/Graphics/Drawable.h>"
C.include "<Urho3D/Graphics/Material.h>"
C.using "namespace Urho3D"

C.verbatim "typedef Vector<SourceBatch> VectorSourceBatch;"
C.verbatim "typedef PODVector<Light*> PODVectorLightPtr;"
C.verbatim "typedef PODVector<Drawable*> PODVectorDrawablePtr;"
C.verbatim "typedef SharedPtr<Material> SharedMaterial;"

podVectorPtr "Drawable"

drawableContext :: C.Context
drawableContext = drawableCntx <> componentContext <> stringHashContext <> vectorSourceBatchCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''Component] ''Drawable

instance NodeComponent Drawable where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Drawable::GetTypeStatic().Value() } |]

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

instance Storable SourceBatch where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(SourceBatch) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<SourceBatch>::AlignmentOf } |]
  peek ptr = do
    _sourceBatchDistance <- realToFrac <$> [C.exp| float {$(SourceBatch* ptr)->distance_}|]
    _sourceBatchGeometry <- [C.exp| Geometry* {$(SourceBatch* ptr)->geometry_}|]
    _sourceBatchMaterial <- peekSharedPtr =<< [C.exp| SharedMaterial* {new SharedPtr<Material>($(SourceBatch* ptr)->material_)}|]
    _sourceBatchWorldTransform <- [C.exp| const Matrix3x4* {$(SourceBatch* ptr)->worldTransform_}|]
    _sourceBatchNumWorldTransforms <- fromIntegral <$> [C.exp| unsigned int {$(SourceBatch* ptr)->numWorldTransforms_}|]
    _sourceBatchGeometryType <- toEnum . fromIntegral <$> [C.exp| int {(int)$(SourceBatch* ptr)->geometryType_}|]
    return SourceBatch {..}
  poke ptr (SourceBatch {..}) = [C.block| void {
      $(SourceBatch* ptr)->distance_ = $(float _sourceBatchDistance');
      $(SourceBatch* ptr)->geometry_ = $(Geometry* _sourceBatchGeometry');
      $(SourceBatch* ptr)->material_ = SharedPtr<Material>($(Material* _sourceBatchMaterial'));
      $(SourceBatch* ptr)->worldTransform_ = $(Matrix3x4* _sourceBatchWorldTransform');
      $(SourceBatch* ptr)->numWorldTransforms_ = $(unsigned int _sourceBatchNumWorldTransforms');
      $(SourceBatch* ptr)->geometryType_ = (GeometryType)$(int _sourceBatchGeometryType');
    } |]
    where
    _sourceBatchDistance' = realToFrac _sourceBatchDistance
    _sourceBatchGeometry' = _sourceBatchGeometry
    _sourceBatchMaterial' = parentPointer _sourceBatchMaterial
    _sourceBatchWorldTransform' = _sourceBatchWorldTransform
    _sourceBatchNumWorldTransforms' = fromIntegral _sourceBatchNumWorldTransforms
    _sourceBatchGeometryType' = fromIntegral . fromEnum $ _sourceBatchGeometryType

instance Storable FrameInfo where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(FrameInfo) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<FrameInfo>::AlignmentOf } |]
  peek ptr = do
      _frameInfoFrameNumber <- fromIntegral <$> [C.exp| unsigned int {$(FrameInfo* ptr)->frameNumber_} |]
      _frameInfoTimeStep <- realToFrac <$> [C.exp| float {$(FrameInfo* ptr)->timeStep_} |]
      _frameInfoViewSize <- peek =<< [C.exp| IntVector2* {&$(FrameInfo* ptr)->viewSize_} |]
      _frameInfoCamera <- [C.exp| Camera* {$(FrameInfo* ptr)->camera_} |]
      return FrameInfo {..}
  poke ptr (FrameInfo {..}) = with _frameInfoViewSize $ \_frameInfoViewSize' ->
    [C.block| void {
      $(FrameInfo* ptr)->frameNumber_ = $(unsigned int _frameInfoFrameNumber');
      $(FrameInfo* ptr)->timeStep_ = $(float _frameInfoTimeStep');
      $(FrameInfo* ptr)->viewSize_ = *$(IntVector2* _frameInfoViewSize');
      $(FrameInfo* ptr)->camera_ = $(Camera* _frameInfoCamera');
     } |]
    where

    _frameInfoFrameNumber' = fromIntegral _frameInfoFrameNumber
    _frameInfoTimeStep' = realToFrac _frameInfoTimeStep
    _frameInfoCamera' = _frameInfoCamera

simpleVector "SourceBatch"

-- | Set draw distance.
drawableSetDrawDistance :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Float -- ^ distance
  -> m ()
drawableSetDrawDistance p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void { $(Drawable* ptr)->SetDrawDistance($(float d')) } |]

-- | Set shadow draw distance.
drawableSetShadowDistance :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Float -- ^ distance
  -> m ()
drawableSetShadowDistance p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void { $(Drawable* ptr)->SetShadowDistance($(float d')) } |]

-- | Set LOD bias.
drawableSetLodBias :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Float -- ^ bias
  -> m ()
drawableSetLodBias p b = liftIO $ do
  let ptr = parentPointer p
      b' = realToFrac b
  [C.exp| void { $(Drawable* ptr)->SetLodBias($(float b')) } |]

-- | Set view mask. Is and'ed with camera's view mask to see if the object should be rendered.
drawableSetViewMask :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Word -- ^ mask
  -> m ()
drawableSetViewMask p m = liftIO $ do
  let ptr = parentPointer p
      m' = fromIntegral m
  [C.exp| void { $(Drawable* ptr)->SetViewMask($(unsigned int m')) } |]

-- | Set light mask. Is and'ed with light's and zone's light mask to see if the object should be lit.
drawableSetLightMask :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Word -- ^ mask
  -> m ()
drawableSetLightMask p m = liftIO $ do
  let ptr = parentPointer p
      m' = fromIntegral m
  [C.exp| void { $(Drawable* ptr)->SetLightMask($(unsigned int m')) } |]

-- | Set shadow mask. Is and'ed with light's light mask and zone's shadow mask to see if the object should be rendered to a shadow map.
drawableSetShadowMask :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Word -- ^ mask
  -> m ()
drawableSetShadowMask p m = liftIO $ do
  let ptr = parentPointer p
      m' = fromIntegral m
  [C.exp| void { $(Drawable* ptr)->SetShadowMask($(unsigned int m')) } |]

-- | Set zone mask. Is and'ed with zone's zone mask to see if the object should belong to the zone.
drawableSetZoneMask :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Word -- ^ mask
  -> m ()
drawableSetZoneMask p m = liftIO $ do
  let ptr = parentPointer p
      m' = fromIntegral m
  [C.exp| void { $(Drawable* ptr)->SetZoneMask($(unsigned int m')) } |]

-- | Set maximum number of per-pixel lights. Default 0 is unlimited.
drawableSetMaxLights :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Word -- ^ num
  -> m ()
drawableSetMaxLights p n = liftIO $ do
  let ptr = parentPointer p
      n' = fromIntegral n
  [C.exp| void { $(Drawable* ptr)->SetMaxLights($(unsigned int n')) } |]

-- | Set shadowcaster flag.
drawableSetCastShadows :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Bool -- ^ enable
  -> m ()
drawableSetCastShadows p e = liftIO $ do
  let ptr = parentPointer p
      e' = fromBool e
  [C.exp| void { $(Drawable* ptr)->SetCastShadows($(int e') != 0) } |]

-- | Set occlusion flag.
drawableSetOccluder :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Bool -- ^ enable
  -> m ()
drawableSetOccluder p e = liftIO $ do
  let ptr = parentPointer p
      e' = fromBool e
  [C.exp| void { $(Drawable* ptr)->SetOccluder($(int e') != 0) } |]

-- | Set occludee flag.
drawableSetOccludee :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Bool -- ^ enable
  -> m ()
drawableSetOccludee p e = liftIO $ do
  let ptr = parentPointer p
      e' = fromBool e
  [C.exp| void { $(Drawable* ptr)->SetOccludee($(int e') != 0) } |]

-- | Mark for update and octree reinsertion. Update is automatically queued when the drawable's scene node moves or changes scale.
drawableMarkForUpdate :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m ()
drawableMarkForUpdate p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Drawable* ptr)->MarkForUpdate() } |]

-- | Return local space bounding box. May not be applicable or properly updated on all drawables.
drawableGetBoundingBox :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m BoundingBox
drawableGetBoundingBox p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const BoundingBox* { &$(Drawable* ptr)->GetBoundingBox() } |]

-- | Return world-space bounding box.
drawableGetWorldBoundingBox :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m BoundingBox
drawableGetWorldBoundingBox p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const BoundingBox* { &$(Drawable* ptr)->GetWorldBoundingBox() } |]

-- | Return drawable flags.
drawableGetDrawableFlags :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Word8
drawableGetDrawableFlags p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned char { $(Drawable* ptr)->GetDrawableFlags() } |]

-- | Return draw distance.
drawableGetDrawDistance :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Float
drawableGetDrawDistance p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Drawable* ptr)->GetDrawDistance() } |]

-- | Return shadow draw distance.
drawableGetShadowDistance :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Float
drawableGetShadowDistance p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Drawable* ptr)->GetShadowDistance() } |]

-- | Return LOD bias.
drawableGetLodBias :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Float
drawableGetLodBias p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Drawable* ptr)->GetLodBias() } |]

-- | Return view mask.
drawableGetViewMask :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Word
drawableGetViewMask p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Drawable* ptr)->GetViewMask() } |]

-- | Return light mask.
drawableGetLightMask :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Word
drawableGetLightMask p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Drawable* ptr)->GetLightMask() } |]

-- | Return shadow mask.
drawableGetShadowMask :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Word
drawableGetShadowMask p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Drawable* ptr)->GetShadowMask() } |]

-- | Return zone mask.
drawableGetZoneMask :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Word
drawableGetZoneMask p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Drawable* ptr)->GetZoneMask() } |]

-- | Return maximum number of per-pixel lights.
drawableGetMaxLights :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Word
drawableGetMaxLights p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Drawable* ptr)->GetMaxLights() } |]

-- | Return shadowcaster flag.
drawableGetCastShadows :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Bool
drawableGetCastShadows p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int) $(Drawable* ptr)->GetCastShadows() } |]

-- | Return occluder flag.
drawableIsOccluder :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Bool
drawableIsOccluder p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int) $(Drawable* ptr)->IsOccluder() } |]

-- | Return occludee flag.
drawableIsOccludee :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Bool
drawableIsOccludee p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int) $(Drawable* ptr)->IsOccludee() } |]

-- | Return whether is in view this frame from any viewport camera. Excludes shadow map cameras.
drawableIsInView :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Bool
drawableIsInView p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int) $(Drawable* ptr)->IsInView() } |]

-- | Return whether is in view of a specific camera this frame. Pass in a null camera to allow any camera, including shadow map cameras.
drawableIsInViewCam :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Ptr Camera -- ^ camera
  -> m Bool
drawableIsInViewCam p ptrcam = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int) $(Drawable* ptr)->IsInView($(Camera* ptrcam)) } |]

-- | Return draw call source data.
drawableGetBatches :: (Parent Drawable a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v SourceBatch)
  => p -- ^ Pointer to Drawable or ascentor
  -> m (v SourceBatch)
drawableGetBatches p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const VectorSourceBatch* { &$(Drawable* ptr)->GetBatches() } |]

-- | Set new zone. Zone assignment may optionally be temporary, meaning it needs to be re-evaluated on the next frame.
drawableSetZone :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Ptr Zone -- ^ zone
  -> Bool -- ^ temporary (def false)
  -> m ()
drawableSetZone p zone t = liftIO $ do
  let ptr = parentPointer p
      t' = fromBool t
  [C.exp| void { $(Drawable* ptr)->SetZone($(Zone* zone), $(int t') != 0) } |]

-- | Set sorting value.
drawableSetSortValue :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Float -- ^ value
  -> m ()
drawableSetSortValue p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(Drawable* ptr)->SetSortValue($(float v')) } |]

-- | Set view-space depth bounds.
drawableSetMinMaxZ :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Float -- ^ min z
  -> Float -- ^ max z
  -> m ()
drawableSetMinMaxZ p minz maxz = liftIO $ do
  let ptr = parentPointer p
      minz' = realToFrac minz
      maxz' = realToFrac maxz
  [C.exp| void { $(Drawable* ptr)->SetMinMaxZ($(float minz'), $(float maxz')) } |]

-- | Mark in view. Also clear the light list.
drawableMarkInView :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> FrameInfo -- ^ frame
  -> m ()
drawableMarkInView p fi = liftIO $ with fi $ \fi' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Drawable* ptr)->MarkInView(*$(FrameInfo* fi')) } |]

-- | Mark in view without specifying a camera. Used for shadow casters.
drawableMarkInView' :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Word -- ^ frame number
  -> m ()
drawableMarkInView' p fn = liftIO $ do
  let ptr = parentPointer p
      fn' = fromIntegral fn
  [C.exp| void { $(Drawable* ptr)->MarkInView($(unsigned int fn')) } |]

-- | Sort and limit per-pixel lights to maximum allowed. Convert extra lights into vertex lights.
drawableLimitLights :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m ()
drawableLimitLights p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Drawable* ptr)->LimitLights() } |]

-- | Sort and limit per-vertex lights to maximum allowed.
drawableLimitVertexLights :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Bool -- ^ remove converted lights
  -> m ()
drawableLimitVertexLights p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(Drawable* ptr)->LimitVertexLights($(int b') != 0) } |]

-- | Set base pass flag for a batch.
drawableSetBasePass :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Word -- ^ batch index
  -> m ()
drawableSetBasePass p bi = liftIO $ do
  let ptr = parentPointer p
      bi' = fromIntegral bi
  [C.exp| void { $(Drawable* ptr)->SetBasePass($(unsigned int bi')) } |]

-- | Return octree octant.
drawableGetOctant :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m (Ptr Octant)
drawableGetOctant p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Octant* { $(Drawable* ptr)->GetOctant() } |]

-- | Return current zone.
drawableGetZone :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m (Ptr Zone)
drawableGetZone p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Zone* { $(Drawable* ptr)->GetZone() } |]

-- | Return whether current zone is inconclusive or dirty due to the drawable moving.
drawableIsZoneDirty :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Bool
drawableIsZoneDirty p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Drawable* ptr)->IsZoneDirty() } |]

-- | Return distance from camera.
drawableGetDistance :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Float
drawableGetDistance p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Drawable* ptr)->GetDistance() } |]

-- | Return LOD scaled distance from camera.
drawableGetLodDistance :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Float
drawableGetLodDistance p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Drawable* ptr)->GetLodDistance() } |]

-- | Return sorting value.
drawableGetSortValue :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Float
drawableGetSortValue p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Drawable* ptr)->GetSortValue() } |]

-- | Return whether is in view on the current frame. Called by View.
drawableIsInViewFrame :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> FrameInfo -- ^ frame
  -> Bool -- ^ any camera (def false)
  -> m Bool
drawableIsInViewFrame p f ac = liftIO $ with f $ \f' -> do
  let ptr = parentPointer p
      ac' = fromBool ac
  toBool <$> [C.exp| int { (int)$(Drawable* ptr)->IsInView(*$(FrameInfo* f'), $(int ac') != 0) } |]

-- | Return whether has a base pass.
drawableHasBasePass :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Word -- ^ batch index
  -> m Bool
drawableHasBasePass p bi = liftIO $ do
  let ptr = parentPointer p
      bi' = fromIntegral bi
  toBool <$> [C.exp| int { (int)$(Drawable* ptr)->HasBasePass($(unsigned int bi')) } |]

-- | Return per-pixel lights.
drawableGetLights :: (Parent Drawable a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v (Ptr Light))
  => p -- ^ Pointer to Drawable or ascentor
  -> m (v (Ptr Light))
drawableGetLights p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const PODVectorLightPtr* { &$(Drawable* ptr)->GetLights() } |]

-- | Return per-vertex lights.
drawableGetVertexLights :: (Parent Drawable a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v (Ptr Light))
  => p -- ^ Pointer to Drawable or ascentor
  -> m (v (Ptr Light))
drawableGetVertexLights p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const PODVectorLightPtr* { &$(Drawable* ptr)->GetVertexLights() } |]

-- | Return the first added per-pixel light.
drawableGetFirstLight :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m (Ptr Light)
drawableGetFirstLight p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Light* { $(Drawable* ptr)->GetFirstLight() } |]

-- | Return the minimum view-space depth.
drawableGetMinZ :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Float
drawableGetMinZ p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Drawable* ptr)->GetMinZ() } |]

-- | Return the maximum view-space depth.
drawableGetMaxZ :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> m Float
drawableGetMaxZ p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Drawable* ptr)->GetMaxZ() } |]

-- | Add a per-pixel light affecting the object this frame.
drawableAddLight :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Ptr Light
  -> m ()
drawableAddLight p plight = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Drawable* ptr)->AddLight($(Light* plight)) } |]

-- | Add a per-vertex light affecting the object this frame.
drawableAddVertexLight :: (Parent Drawable a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Drawable or ascentor
  -> Ptr Light
  -> m ()
drawableAddVertexLight p plight= liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Drawable* ptr)->AddVertexLight($(Light* plight)) } |]
