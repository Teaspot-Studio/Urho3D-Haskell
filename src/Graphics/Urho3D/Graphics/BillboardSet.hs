{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.BillboardSet(
    BillboardSet
  , Billboard(..)
  , HasPosition(..)
  , HasSize(..)
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

import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Graphics.Material
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Math.StringHash
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
  <> vectorContext
  <> rectContext
  <> colorContext
  <> materialContext
  <> variantContext )

C.include "<Urho3D/Graphics/BillboardSet.h>"
C.using "namespace Urho3D"

C.verbatim "typedef Vector<Variant> VectorVariant;"
C.verbatim "typedef PODVector<unsigned char> PODVectorWord8;"
C.verbatim "typedef PODVector<Billboard> PODVectorBillboard;"

billboardSetContext :: C.Context
billboardSetContext = componentContext <> billboardSetCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Drawable] ''BillboardSet

instance Creatable (Ptr BillboardSet) where
  type CreationOptions (Ptr BillboardSet) = Ptr Context

  newObject ptr = liftIO $ [C.exp| BillboardSet* { new BillboardSet($(Context* ptr)) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(BillboardSet* ptr) } |]

instance NodeComponent BillboardSet where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { BillboardSet::GetTypeStatic().Value() } |]

-- | Set material
billboardSetSetMaterial :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Ptr Material -- ^ Material to set
  -> m ()
billboardSetSetMaterial p pmaterial = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(BillboardSet* ptr)->SetMaterial($(Material* pmaterial)) } |]

-- | Set number of billboards.
billboardSetSetNumBillboards :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Int -- ^ Number of billboards
  -> m ()
billboardSetSetNumBillboards p n = liftIO $ do
  let ptr = parentPointer p
      n' = fromIntegral n
  [C.exp| void { $(BillboardSet* ptr)->SetNumBillboards($(unsigned int n')) } |]
--void SetNumBillboards(unsigned num);

-- | Set whether billboards are relative to the scene node. Default true.
billboardSetSetRelative :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Bool -- ^ Enable?
  -> m ()
billboardSetSetRelative p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(BillboardSet* ptr)->SetRelative($(int v') != 0) } |]
-- void SetRelative(bool enable);

-- | Set whether scene node scale affects billboards' size. Default true.
billboardSetSetScaled :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Bool -- ^ Enable?
  -> m ()
billboardSetSetScaled p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(BillboardSet* ptr)->SetScaled($(int v') != 0) } |]
-- void SetScaled(bool enable);

-- | Set whether billboards are sorted by distance. Default false.
billboardSetSetSorted :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Bool -- ^ Enable?
  -> m ()
billboardSetSetSorted p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(BillboardSet* ptr)->SetSorted($(int v') != 0) } |]
-- void SetSorted(bool enable);

-- | Set whether billboards have fixed size on screen (measured in pixels) regardless of distance to camera. Default false.
billboardSetSetFixedScreenSize :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Bool -- ^ Enable?
  -> m ()
billboardSetSetFixedScreenSize p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(BillboardSet* ptr)->SetFixedScreenSize($(int v') != 0) } |]
-- void SetFixedScreenSize(bool enable);

-- | Set how the billboards should rotate in relation to the camera. Default is to follow camera rotation on all axes (FC_ROTATE_XYZ.)
billboardSetSetFaceCameraMode :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> FaceCameraMode
  -> m ()
billboardSetSetFaceCameraMode p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral . fromEnum $ v
  [C.exp| void { $(BillboardSet* ptr)->SetFaceCameraMode((FaceCameraMode)$(int v')) } |]
--void SetFaceCameraMode(FaceCameraMode mode);

-- | Set minimal angle between billboard normal and look-at direction.
billboardSetSetMinAngle :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Float -- ^ Angle
  -> m ()
billboardSetSetMinAngle p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(BillboardSet* ptr)->SetMinAngle($(float v')) } |]
--void SetMinAngle(float angle);

-- | Set animation LOD bias.
billboardSetSetAnimationLodBias :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Float -- ^ bias
  -> m ()
billboardSetSetAnimationLodBias p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(BillboardSet* ptr)->SetAnimationLodBias($(float v')) } |]
-- void SetAnimationLodBias(float bias);

-- | Mark for bounding box and vertex buffer update. Call after modifying the billboards.
billboardSetCommit :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m ()
billboardSetCommit p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(BillboardSet* ptr)->Commit() } |]
-- void Commit();

-- | Return material.
billboardSetGetMaterial :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m (Ptr Material)
billboardSetGetMaterial p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Material* { $(BillboardSet* ptr)->GetMaterial() } |]
-- Material* GetMaterial() const;

-- | Return number of billboards.
billboardSetGetNumBillboards :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Int
billboardSetGetNumBillboards p = liftIO $ do
  let ptr = parentPointer p
  fmap fromIntegral [C.exp| int { $(BillboardSet* ptr)->GetNumBillboards() } |]
--unsigned GetNumBillboards() const { return billboards_.Size(); }

-- | Return all billboards.
billboardSetGetBillboards :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v Billboard)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m (v Billboard)
billboardSetGetBillboards p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| PODVectorBillboard* { &$(BillboardSet* ptr)->GetBillboards() } |]
-- PODVector<Billboard>& GetBillboards() { return billboards_; }

-- | Return billboard by index.
billboardSetGetBillboard :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Int -- ^ Number
  -> m Billboard
billboardSetGetBillboard p n = liftIO $ do
  let ptr = parentPointer p
      n' = fromIntegral n
  peek =<< [C.exp| Billboard* { $(BillboardSet* ptr)->GetBillboard($(unsigned int n')) } |]
-- Billboard* GetBillboard(unsigned index);

-- | Set billboard data
billboardSetSetBillboard :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> Int -- ^ Number
  -> Billboard -- ^ Billboard datum
  -> m ()
billboardSetSetBillboard p n b = liftIO $ do
  let ptr = parentPointer p
      n' = fromIntegral n
  bptr <- [C.exp| Billboard* { $(BillboardSet* ptr)->GetBillboard($(unsigned int n')) } |]
  poke bptr b

-- | Return whether billboards are relative to the scene node.
billboardSetIsRelative :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Bool
billboardSetIsRelative p = liftIO $ do
  let ptr = parentPointer p
  fmap toBool [C.exp| int { (int)$(BillboardSet* ptr)->IsRelative() } |]
-- bool IsRelative() const { return relative_; }

-- | Return whether scene node scale affects billboards' size.
billboardSetIsScaled :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Bool
billboardSetIsScaled p = liftIO $ do
  let ptr = parentPointer p
  fmap toBool [C.exp| int { (int)$(BillboardSet* ptr)->IsScaled() } |]
-- bool IsScaled() const { return scaled_; }

-- | Return whether billboards are sorted.
billboardSetIsSorted :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Bool
billboardSetIsSorted p = liftIO $ do
  let ptr = parentPointer p
  fmap toBool [C.exp| int { (int)$(BillboardSet* ptr)->IsSorted() } |]
-- bool IsSorted() const { return sorted_; }

-- | Return whether billboards are fixed screen size.
billboardSetIsFixedScreenSize :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Bool
billboardSetIsFixedScreenSize p = liftIO $ do
  let ptr = parentPointer p
  fmap toBool [C.exp| int { (int)$(BillboardSet* ptr)->IsFixedScreenSize() } |]
-- bool IsFixedScreenSize() const { return fixedScreenSize_; }

-- | Return how the billboards rotate in relation to the camera.
billboardSetGetFaceCameraMode :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m FaceCameraMode
billboardSetGetFaceCameraMode p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int { (int)$(BillboardSet* ptr)->GetFaceCameraMode() } |]
-- FaceCameraMode GetFaceCameraMode() const { return faceCameraMode_; }

-- | Return minimal angle between billboard normal and look-at direction.
billboardSetGetMinAngle :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Float
billboardSetGetMinAngle p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { (float)$(BillboardSet* ptr)->GetMinAngle() } |]
-- float GetMinAngle() const { return minAngle_; }

-- | Return animation LOD bias.
billboardSetGetAnimationLodBias :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m Float
billboardSetGetAnimationLodBias p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { (float)$(BillboardSet* ptr)->GetAnimationLodBias() } |]
-- float GetAnimationLodBias() const { return animationLodBias_; }

-- | Set material attribute.
billboardSetSetMaterialAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> ResourceRef -- ^ Value
  -> m ()
billboardSetSetMaterialAttr p r = liftIO $ with r $ \r' -> do
  let ptr = parentPointer p
  [C.exp| void { $(BillboardSet* ptr)->SetMaterialAttr(*$(ResourceRef* r')) } |]
-- void SetMaterialAttr(const ResourceRef& value);

-- | Set billboards attribute.
billboardSetSetBillboardsAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v (Ptr Variant))
  => p -- ^ Pointer to BillboardSet or ascentor
  -> v (Ptr Variant)
  -> m ()
billboardSetSetBillboardsAttr p v = liftIO $ withForeignVector () v $ \(v' :: Ptr VectorVariant) -> do
  let ptr = parentPointer p
  [C.exp| void { $(BillboardSet* ptr)->SetBillboardsAttr(*$(VectorVariant* v')) } |]
-- void SetBillboardsAttr(const VariantVector& value);

-- | Set billboards attribute for network replication.
billboardSetSetNetBillboardsAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v Word8)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> v Word8
  -> m ()
billboardSetSetNetBillboardsAttr p v = liftIO $ withForeignVector () v $ \(v' :: Ptr PODVectorWord8) -> do
  let ptr = parentPointer p
  [C.exp| void { $(BillboardSet* ptr)->SetNetBillboardsAttr(*$(PODVectorWord8* v')) } |]
-- void SetNetBillboardsAttr(const PODVector<unsigned char>& value);

-- | Return material attribute.
billboardSetGetMaterialAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m ResourceRef
billboardSetGetMaterialAttr p = liftIO $ alloca $ \r -> do
  let ptr = parentPointer p
  [C.exp| void { *($(ResourceRef* r)) = $(BillboardSet* ptr)->GetMaterialAttr() } |]
  peek r
-- ResourceRef GetMaterialAttr() const;

-- | Return billboards attribute.
billboardSetGetBillboardsAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v (Ptr Variant))
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m (v (Ptr Variant))
billboardSetGetBillboardsAttr p = liftIO $ do
  let ptr = parentPointer p
  resptr <- [C.exp| VectorVariant* {
    new VectorVariant($(BillboardSet* ptr)->GetBillboardsAttr())
    } |]
  v <- peekForeignVectorAs resptr
  [C.exp| void { delete $(VectorVariant* resptr) }|]
  pure v
-- VariantVector GetBillboardsAttr() const;

-- | Return billboards attribute for network replication.
billboardSetGetNetBillboardsAttr :: (Parent BillboardSet a, Pointer p a, MonadIO m, ForeignVector v Word8)
  => p -- ^ Pointer to BillboardSet or ascentor
  -> m (v Word8)
billboardSetGetNetBillboardsAttr p = liftIO $ do
  let ptr = parentPointer p
  resptr <- [C.exp| PODVectorWord8* {
    new PODVectorWord8($(BillboardSet* ptr)->GetNetBillboardsAttr())
    } |]
  v <- peekForeignVectorAs resptr
  [C.exp| void { delete $(PODVectorWord8* resptr) }|]
  pure v
-- const PODVector<unsigned char>& GetNetBillboardsAttr() const;
