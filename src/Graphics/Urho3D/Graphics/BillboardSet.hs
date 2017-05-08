{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.BillboardSet(
    BillboardSet
  , Billboard(..)
  , HasPosition(..)
  , HasUv(..)
  , HasColor(..)
  , HasRotation(..)
  , HasDirection(..)
  , HasEnabled(..)
  , HasSortDistance(..)
  , HasScreenScaleFactor(..)
  , billboardSetContext
  , billboardSetSetMaterial
  , billboardSetSetNumBillboards
  , billboardSetSetRelative
  , billboardSetSetScaled
  , billboardSetSetSorted
  , billboardSetSetFixedScreenSize
  , billboardSetSetFaceCameraMode
  , billboardSetSetMinAngle
  , billboardSetSetAnimationLodBias
  , billboardSetCommit
  , billboardSetGetMaterial
  , billboardSetGetNumBillboards
  , billboardSetGetBillboards
  , billboardSetGetBillboard
  , billboardSetSetBillboard
  , billboardSetIsRelative
  , billboardSetIsScaled
  , billboardSetIsSorted
  , billboardSetIsFixedScreenSize
  , billboardSetGetFaceCameraMode
  , billboardSetGetMinAngle
  , billboardSetGetAnimationLodBias
  , billboardSetSetMaterialAttr
  , billboardSetSetBillboardsAttr
  , billboardSetSetNetBillboardsAttr
  , billboardSetGetMaterialAttr
  , billboardSetGetBillboardsAttr
  , billboardSetGetNetBillboardsAttr
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Graphics.Drawable
import Graphics.Urho3D.Graphics.Internal.BillboardSet
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Scene.Node
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ

import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Graphics.Material
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx
  <> contextContext
  <> billboardSetCntx
  <> drawableContext
  <> componentContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> vector3Context
  <> vector2Context
  <> rectContext
  <> colorContext
  <> materialContext
  <> variantContext )

C.include "<Urho3D/Graphics/BillboardSet.h>"
C.using "namespace Urho3D"

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

C.verbatim "typedef Vector<Variant> VectorVariant;"
C.verbatim "typedef PODVector<unsigned char> PODVectorWord8;"

billboardSetContext :: C.Context
billboardSetContext = componentContext <> billboardSetCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Drawable] ''BillboardSet

instance Creatable (Ptr BillboardSet) where
  type CreationOptions (Ptr BillboardSet) = Ptr Context

  newObject ptr = liftIO $ [C.exp| BillboardSet* { new BillboardSet($(Context* ptr)) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(BillboardSet* ptr) } |]

instance NodeComponent BillboardSet where
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = BillboardSet::GetTypeStatic();
    return &h;
  } |]

instance Storable Billboard where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Billboard) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Billboard>::AlignmentOf } |]
  peek ptr = do
    _billboardPosition <- peek =<< [C.exp| Vector3* {&$(Billboard* ptr)->position_} |]
    _billboardSize <- peek =<< [C.exp| Vector2* {&$(Billboard* ptr)->size_} |]
    _billboardUv <- peek =<< [C.exp| Rect* {&$(Billboard* ptr)->uv_} |]
    _billboardColor <- peek =<< [C.exp| Color* {&$(Billboard* ptr)->color_} |]
    _billboardRotation <- realToFrac <$> [C.exp| float {$(Billboard* ptr)->rotation_} |]
    _billboardDirection <- peek =<< [C.exp| Vector3* {&$(Billboard* ptr)->direction_} |]
    _billboardEnabled <- toBool <$> [C.exp| int {$(Billboard* ptr)->enabled_} |]
    _billboardSortDistance <- realToFrac <$> [C.exp| float {$(Billboard* ptr)->sortDistance_} |]
    _billboardScreenScaleFactor <- realToFrac <$> [C.exp| float {$(Billboard* ptr)->screenScaleFactor_} |]
    return Billboard {..}
  poke ptr (Billboard {..}) =
    with _billboardPosition $ \_billboardPosition' ->
    with _billboardSize $ \_billboardSize' ->
    with _billboardUv $ \_billboardUv' ->
    with _billboardColor $ \_billboardColor' ->
    with _billboardDirection $ \_billboardDirection' ->
    [C.block| void {
      $(Billboard* ptr)->position_ = *$(Vector3* _billboardPosition');
      $(Billboard* ptr)->size_ = *$(Vector2* _billboardSize');
      $(Billboard* ptr)->uv_ = *$(Rect* _billboardUv');
      $(Billboard* ptr)->color_ = *$(Color* _billboardColor');
      $(Billboard* ptr)->rotation_ = $(float _billboardRotation');
      $(Billboard* ptr)->direction_ = *$(Vector3* _billboardDirection');
      $(Billboard* ptr)->enabled_ = $(int _billboardEnabled') != 0;
      $(Billboard* ptr)->sortDistance_ = $(float _billboardSortDistance');
      $(Billboard* ptr)->screenScaleFactor_ = $(float _billboardScreenScaleFactor');
    } |]
    where
    _billboardRotation' = realToFrac _billboardRotation
    _billboardEnabled' = fromBool _billboardEnabled
    _billboardSortDistance' = realToFrac _billboardSortDistance
    _billboardScreenScaleFactor' = realToFrac _billboardScreenScaleFactor

-- | Set material
billboardSetSetMaterial :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Ptr Material -- ^ Material to set
  -> m ()
billboardSetSetMaterial = undefined

-- | Set number of billboards.
billboardSetSetNumBillboards :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Int -- ^ Number of billboards
  -> m ()
billboardSetSetNumBillboards = undefined
--void SetNumBillboards(unsigned num);

-- | Set whether billboards are relative to the scene node. Default true.
billboardSetSetRelative :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Bool -- ^ Enable?
  -> m ()
billboardSetSetRelative = undefined
-- void SetRelative(bool enable);

-- | Set whether scene node scale affects billboards' size. Default true.
billboardSetSetScaled :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Bool -- ^ Enable?
  -> m ()
billboardSetSetScaled = undefined
-- void SetScaled(bool enable);

-- | Set whether billboards are sorted by distance. Default false.
billboardSetSetSorted :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Bool -- ^ Enable?
  -> m ()
billboardSetSetSorted = undefined
-- void SetSorted(bool enable);

-- | Set whether billboards have fixed size on screen (measured in pixels) regardless of distance to camera. Default false.
billboardSetSetFixedScreenSize :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Bool -- ^ Enable?
  -> m ()
billboardSetSetFixedScreenSize = undefined
-- void SetFixedScreenSize(bool enable);

-- | Set how the billboards should rotate in relation to the camera. Default is to follow camera rotation on all axes (FC_ROTATE_XYZ.)
billboardSetSetFaceCameraMode :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> FaceCameraMode
  -> m ()
billboardSetSetFaceCameraMode = undefined
--void SetFaceCameraMode(FaceCameraMode mode);

-- | Set minimal angle between billboard normal and look-at direction.
billboardSetSetMinAngle :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Float -- ^ Angle
  -> m ()
billboardSetSetMinAngle = undefined
--void SetMinAngle(float angle);

-- | Set animation LOD bias.
billboardSetSetAnimationLodBias :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Float -- ^ bias
  -> m ()
billboardSetSetAnimationLodBias = undefined
-- void SetAnimationLodBias(float bias);

-- | Mark for bounding box and vertex buffer update. Call after modifying the billboards.
billboardSetCommit :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m ()
billboardSetCommit = undefined
-- void Commit();

-- | Return material.
billboardSetGetMaterial :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m (Ptr Material)
billboardSetGetMaterial = undefined
-- Material* GetMaterial() const;

-- | Return number of billboards.
billboardSetGetNumBillboards :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Int
billboardSetGetNumBillboards = undefined
--unsigned GetNumBillboards() const { return billboards_.Size(); }

-- | Return all billboards.
billboardSetGetBillboards :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v Billboard)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m (v Billboard)
billboardSetGetBillboards = undefined
-- PODVector<Billboard>& GetBillboards() { return billboards_; }

-- | Return billboard by index.
billboardSetGetBillboard :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Int -- ^ Number
  -> m Billboard
billboardSetGetBillboard = undefined
-- Billboard* GetBillboard(unsigned index);

-- | Set billboard data
billboardSetSetBillboard :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Int -- ^ Number
  -> Billboard -- ^ Billboard datum
  -> m ()
billboardSetSetBillboard = undefined

-- | Return whether billboards are relative to the scene node.
billboardSetIsRelative :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Bool
billboardSetIsRelative = undefined
-- bool IsRelative() const { return relative_; }

-- | Return whether scene node scale affects billboards' size.
billboardSetIsScaled :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Bool
billboardSetIsScaled = undefined
-- bool IsScaled() const { return scaled_; }

-- | Return whether billboards are sorted.
billboardSetIsSorted :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Bool
billboardSetIsSorted = undefined
-- bool IsSorted() const { return sorted_; }

-- | Return whether billboards are fixed screen size.
billboardSetIsFixedScreenSize :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Bool
billboardSetIsFixedScreenSize = undefined
-- bool IsFixedScreenSize() const { return fixedScreenSize_; }

-- | Return how the billboards rotate in relation to the camera.
billboardSetGetFaceCameraMode :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m FaceCameraMode
billboardSetGetFaceCameraMode = undefined
-- FaceCameraMode GetFaceCameraMode() const { return faceCameraMode_; }

-- | Return minimal angle between billboard normal and look-at direction.
billboardSetGetMinAngle :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Float
billboardSetGetMinAngle = undefined
-- float GetMinAngle() const { return minAngle_; }

-- | Return animation LOD bias.
billboardSetGetAnimationLodBias :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Float
billboardSetGetAnimationLodBias = undefined
-- float GetAnimationLodBias() const { return animationLodBias_; }

-- | Set material attribute.
billboardSetSetMaterialAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> ResourceRef -- ^ Value
  -> m ()
billboardSetSetMaterialAttr = undefined
-- void SetMaterialAttr(const ResourceRef& value);

-- | Set billboards attribute.
billboardSetSetBillboardsAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v Variant)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> v Variant
  -> m ()
billboardSetSetBillboardsAttr = undefined
-- void SetBillboardsAttr(const VariantVector& value);

-- | Set billboards attribute for network replication.
billboardSetSetNetBillboardsAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v Word8)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> v Word8
  -> m ()
billboardSetSetNetBillboardsAttr = undefined
-- void SetNetBillboardsAttr(const PODVector<unsigned char>& value);

-- | Return material attribute.
billboardSetGetMaterialAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m ResourceRef
billboardSetGetMaterialAttr = undefined
-- ResourceRef GetMaterialAttr() const;

-- | Return billboards attribute.
billboardSetGetBillboardsAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v Variant)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m (v Variant)
billboardSetGetBillboardsAttr = undefined
-- VariantVector GetBillboardsAttr() const;

-- | Return billboards attribute for network replication.
billboardSetGetNetBillboardsAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v Variant)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m (v Word8)
billboardSetGetNetBillboardsAttr = undefined
-- const PODVector<unsigned char>& GetNetBillboardsAttr() const;
