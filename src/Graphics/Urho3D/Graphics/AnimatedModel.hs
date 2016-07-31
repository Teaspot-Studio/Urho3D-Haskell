{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.AnimatedModel(
    AnimatedModel
  , animatedModelContext
  , animatedModelSetModel
  , animatedModelAddAnimationState
  , AnimatedModelRemoveAnimationState(..)
  , animatedModelRemoveAllAnimationStates
  , animatedModelSetAnimationLodBias
  , animatedModelSetUpdateInvisible
  , AnimatedModelSetMorphWeight(..)
  , animatedModelResetMorphWeights
  , animatedModelGetSkeleton
  , animatedModelGetAnimationStates
  , animatedModelGetNumAnimationStates
  , AnimatedModelGetAnimationState(..)
  , animatedModelGetAnimationLodBias
  , animatedModelGetUpdateInvisible
  , animatedModelGetMorphs
  , animatedModelGetMorphVertexBuffers
  , animatedModelGetNumMorphs
  , AnimatedModelGetMorphWeight(..)
  , animatedModelIsMaster
  , animatedModelSetModelAttr
  , animatedModelSetBonesEnabledAttr
  , animatedModelSetAnimationStatesAttr
  , animatedModelSetMorphsAttr
  , animatedModelGetModelAttr
  , animatedModelGetBonesEnabledAttr
  , animatedModelGetAnimationStatesAttr
  , animatedModelGetMorphsAttr
  , animatedModelGetGeometryBoneMappings
  , animatedModelGetGeometrySkinMatrices
  , animatedModelUpdateBoneBoundingBox
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.AnimatedModel
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import Foreign.C.String 
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Graphics.Model 
import Graphics.Urho3D.Graphics.Material
import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Scene.Node

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Graphics.Animation
import Graphics.Urho3D.Graphics.AnimationState
import Graphics.Urho3D.Graphics.Drawable 
import Graphics.Urho3D.Graphics.StaticModel
import Graphics.Urho3D.Graphics.Skeleton
import Graphics.Urho3D.Graphics.ModelMorph
import Graphics.Urho3D.Graphics.VertexBuffer
import Graphics.Urho3D.Parent

C.context (C.cppCtx 
  <> animatedModelCntx 
  <> componentContext 
  <> stringHashContext 
  <> drawableContext 
  <> modelContext 
  <> materialContext 
  <> animatableContext 
  <> serializableContext 
  <> objectContext 
  <> staticModelContext
  <> skeletonContext
  <> animationContext
  <> animationStateContext
  <> modelMorphContext
  <> vertexBufferContext
  <> vectorContext
  <> matrix3x4Context
  <> variantContext
  <> contextContext)

C.include "<Urho3D/Graphics/AnimatedModel.h>"
C.using "namespace Urho3D"

C.verbatim "typedef Vector<SharedPtr<AnimationState> > VectorSharedAnimationStatePtr;"
C.verbatim "typedef Vector<ModelMorph> VectorModelMorph;"
C.verbatim "typedef Vector<SharedPtr<VertexBuffer> > VectorSharedVertexBufferPtr;"
C.verbatim "typedef PODVector<unsigned char> PODVectorWord8;"
C.verbatim "typedef Vector<PODVector<unsigned> > VectorPODVectorWord;"
C.verbatim "typedef Vector<PODVector<Matrix3x4> > VectorPODVectorMatrix3x4;"
C.verbatim "typedef Vector<Variant> VectorVariant;"

animatedModelContext :: C.Context 
animatedModelContext = 
     animatedModelCntx 
  <> componentContext 
  <> stringHashContext

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Drawable, ''StaticModel] ''AnimatedModel

instance NodeComponent AnimatedModel where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = AnimatedModel::GetTypeStatic();
    return &h;
  } |]

instance Creatable (Ptr AnimatedModel) where 
  type CreationOptions (Ptr AnimatedModel) = Ptr Context 

  newObject ptr = liftIO [C.exp| AnimatedModel* { new AnimatedModel($(Context* ptr))} |]
  deleteObject ptr = liftIO [C.exp| void { delete $(AnimatedModel* ptr) } |]

-- | Set model.
animatedModelSetModel :: (Parent AnimatedModel a, Pointer p a, MonadIO m, Parent Model b, Pointer pmodel b) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> pmodel -- ^ Pointer to Model or ascentor
  -> Bool -- ^ Create bones? (def true)
  -> m ()
animatedModelSetModel p pmodel createBones = liftIO $ do 
  let ptr = parentPointer p 
      ptrmodel = parentPointer pmodel 
      createBones' = fromBool createBones
  [C.exp| void { $(AnimatedModel* ptr)->SetModel($(Model* ptrmodel), $(int createBones') != 0) } |]

-- | Add an animation.
animatedModelAddAnimationState :: (Parent AnimatedModel a, Pointer p a, MonadIO m, Parent Animation b, Pointer panimation b) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> panimation -- ^ Pointer to Animation or ascentor
  -> m (Ptr AnimationState)
animatedModelAddAnimationState p panim = liftIO $ do 
  let ptr = parentPointer p 
      ptranim = parentPointer panim
  [C.exp| AnimationState* { $(AnimatedModel* ptr)->AddAnimationState($(Animation* ptranim)) } |]

class AnimatedModelRemoveAnimationState a where 
  -- | Remove an animation
  animatedModelRemoveAnimationState :: (Parent AnimatedModel b, Pointer p b, MonadIO m) 
    => p -- ^ Pointer to AnimatedModel or ascentor
    -> a -- ^ Value by which animation state is identified
    -> m ()

-- | Remove an animation by animation pointer.
instance AnimatedModelRemoveAnimationState (Ptr Animation) where
  animatedModelRemoveAnimationState p ptranim = liftIO $ do 
    let ptr = parentPointer p
    [C.exp| void { $(AnimatedModel* ptr)->RemoveAnimationState($(Animation* ptranim)) } |]

-- | Remove an animation by animation name.
instance AnimatedModelRemoveAnimationState String where
  animatedModelRemoveAnimationState p n = liftIO $ withCString n $ \n' -> do 
    let ptr = parentPointer p 
    [C.exp| void { $(AnimatedModel* ptr)->RemoveAnimationState(String($(const char* n'))) } |]

-- | Remove an animation by animation name hash.
instance AnimatedModelRemoveAnimationState (Ptr StringHash) where
  animatedModelRemoveAnimationState p phash = liftIO $ do 
    let ptr = parentPointer p 
    [C.exp| void { $(AnimatedModel* ptr)->RemoveAnimationState(*$(StringHash* phash)) } |]

-- | Remove an animation by AnimationState pointer.
instance AnimatedModelRemoveAnimationState (Ptr AnimationState) where
  animatedModelRemoveAnimationState p pstate = liftIO $ do 
    let ptr = parentPointer p 
    [C.exp| void { $(AnimatedModel* ptr)->RemoveAnimationState($(AnimationState* pstate)) } |]

-- | Remove an animation by index.
instance AnimatedModelRemoveAnimationState Int where
  animatedModelRemoveAnimationState p i = liftIO $ do 
    let ptr = parentPointer p 
        i' = fromIntegral i
    [C.exp| void { $(AnimatedModel* ptr)->RemoveAnimationState($(unsigned int i')) } |]

-- | Remove all animations.
animatedModelRemoveAllAnimationStates :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m ()
animatedModelRemoveAllAnimationStates p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(AnimatedModel* ptr)->RemoveAllAnimationStates() } |]

-- | Set animation LOD bias.
animatedModelSetAnimationLodBias :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> Float -- ^ bias
  -> m ()
animatedModelSetAnimationLodBias p bias = liftIO $ do 
  let ptr = parentPointer p 
      bias' = realToFrac bias
  [C.exp| void { $(AnimatedModel* ptr)->SetAnimationLodBias($(float bias')) } |]

-- | Set whether to update animation and the bounding box when not visible. Recommended to enable for physically controlled models like ragdolls.
animatedModelSetUpdateInvisible :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> Bool -- ^ Enable?
  -> m ()
animatedModelSetUpdateInvisible p e = liftIO $ do 
  let ptr = parentPointer p 
      e' = fromBool e
  [C.exp| void { $(AnimatedModel* ptr)->SetUpdateInvisible($(int e') != 0) } |]

class AnimatedModelSetMorphWeight a where 
  -- | Set vertex morph weight.
  animatedModelSetMorphWeight :: (Parent AnimatedModel b, Pointer p b, MonadIO m) 
    => p -- ^ Pointer to AnimatedModel or ascentor
    -> a -- ^ Value to identify morph
    -> Float -- ^ weight
    -> m ()

-- | Set vertex morph weight by index.
instance AnimatedModelSetMorphWeight Int where 
  animatedModelSetMorphWeight p i w = liftIO $ do 
    let ptr = parentPointer p 
        i' = fromIntegral i 
        w' = realToFrac w
    [C.exp| void { $(AnimatedModel* ptr)->SetMorphWeight($(int i'), $(float w')) } |]

-- | Set vertex morph weight by name.
instance AnimatedModelSetMorphWeight String where 
  animatedModelSetMorphWeight p n w = liftIO $ withCString n $ \n' -> do 
    let ptr = parentPointer p 
        w' = realToFrac w
    [C.exp| void { $(AnimatedModel* ptr)->SetMorphWeight(String($(const char* n')), $(float w')) } |]

-- | Set vertex morph weight by name hash.
instance AnimatedModelSetMorphWeight (Ptr StringHash) where 
  animatedModelSetMorphWeight p phash w = liftIO $ do 
    let ptr = parentPointer p 
        w' = realToFrac w
    [C.exp| void { $(AnimatedModel* ptr)->SetMorphWeight(*$(StringHash* phash), $(float w')) } |]

-- | Reset all vertex morphs to zero.
animatedModelResetMorphWeights :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m ()
animatedModelResetMorphWeights p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(AnimatedModel* ptr)->ResetMorphWeights() } |]

-- | Return skeleton.
animatedModelGetSkeleton :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m (Ptr Skeleton)
animatedModelGetSkeleton p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Skeleton* { &$(AnimatedModel* ptr)->GetSkeleton() } |]

-- | Return all animation states.
animatedModelGetAnimationStates :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v (SharedPtr AnimationState)) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m (v (SharedPtr AnimationState))
animatedModelGetAnimationStates p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.exp| const VectorSharedAnimationStatePtr* { &$(AnimatedModel* ptr)->GetAnimationStates() } |]

-- | Return number of animation states.
animatedModelGetNumAnimationStates :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m Int
animatedModelGetNumAnimationStates p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int { $(AnimatedModel* ptr)->GetNumAnimationStates() } |]

class AnimatedModelGetAnimationState a where 
  animatedModelGetAnimationState :: (Parent AnimatedModel b, Pointer p b, MonadIO m)
    => p -- ^ Pointer to AnimatedModel or ascentor
    -> a -- ^ Value the animation state is identified by
    -> m (Ptr AnimationState)

-- | Return animation state by animation pointer.
instance AnimatedModelGetAnimationState (Ptr Animation) where 
  animatedModelGetAnimationState p ptranim = liftIO $ do 
    let ptr = parentPointer p 
    [C.exp| AnimationState* { $(AnimatedModel* ptr)->GetAnimationState($(Animation* ptranim)) } |]

-- | Return animation state by animation name.
instance AnimatedModelGetAnimationState String where 
  animatedModelGetAnimationState p n = liftIO $ withCString n $ \n' -> do 
    let ptr = parentPointer p
    [C.exp| AnimationState* { $(AnimatedModel* ptr)->GetAnimationState(String($(const char* n'))) } |]

-- | Return animation state by animation name hash.
instance AnimatedModelGetAnimationState (Ptr StringHash) where 
  animatedModelGetAnimationState p phash = liftIO $ do 
    let ptr = parentPointer p
    [C.exp| AnimationState* { $(AnimatedModel* ptr)->GetAnimationState(*$(StringHash* phash)) } |]

-- | Return animation state by index.
instance AnimatedModelGetAnimationState Int where 
  animatedModelGetAnimationState p i = liftIO $ do 
    let ptr = parentPointer p
        i' = fromIntegral i
    [C.exp| AnimationState* { $(AnimatedModel* ptr)->GetAnimationState($(unsigned int i')) } |]

-- | Return animation LOD bias.
animatedModelGetAnimationLodBias :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m Float
animatedModelGetAnimationLodBias p = liftIO $ do 
  let ptr = parentPointer p 
  realToFrac <$> [C.exp| float { $(AnimatedModel* ptr)->GetAnimationLodBias() } |]

-- | Return whether to update animation when not visible.
animatedModelGetUpdateInvisible :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m Bool
animatedModelGetUpdateInvisible p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int)$(AnimatedModel* ptr)->GetUpdateInvisible() } |]

-- | Return all vertex morphs.
animatedModelGetMorphs :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v ModelMorph) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m (v ModelMorph)
animatedModelGetMorphs p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.exp| const VectorModelMorph* { &$(AnimatedModel* ptr)->GetMorphs() } |]

-- | Return all morph vertex buffers.
animatedModelGetMorphVertexBuffers :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v (SharedPtr VertexBuffer)) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m (v (SharedPtr VertexBuffer))
animatedModelGetMorphVertexBuffers p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.exp| const VectorSharedVertexBufferPtr* { &$(AnimatedModel* ptr)->GetMorphVertexBuffers() } |]

-- | Return number of vertex morphs.
animatedModelGetNumMorphs :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m Int
animatedModelGetNumMorphs p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int { $(AnimatedModel* ptr)->GetNumMorphs() } |]

class AnimatedModelGetMorphWeight a where 
  -- | Return vertex morph weight
  animatedModelGetMorphWeight :: (Parent AnimatedModel b, Pointer p b, MonadIO m) 
    => p -- ^ Pointer to AnimatedModel or ascentor
    -> a -- ^ Identifing value of morph
    -> m Float

instance AnimatedModelGetMorphWeight Int where 
  -- | Return vertex morph weight by index.
  animatedModelGetMorphWeight p i = liftIO $ do 
    let ptr = parentPointer p 
        i' = fromIntegral i
    realToFrac <$> [C.exp| float { $(AnimatedModel* ptr)->GetMorphWeight($(int i')) } |]

instance AnimatedModelGetMorphWeight String where 
  -- | Return vertex morph weight by name.
  animatedModelGetMorphWeight p n = liftIO $ withCString n $ \n' -> do 
    let ptr = parentPointer p 
    realToFrac <$> [C.exp| float { $(AnimatedModel* ptr)->GetMorphWeight(String($(const char* n'))) } |]

instance AnimatedModelGetMorphWeight (Ptr StringHash) where 
  -- | Return vertex morph weight by name hash.
  animatedModelGetMorphWeight p phash = liftIO $ do 
    let ptr = parentPointer p 
    realToFrac <$> [C.exp| float { $(AnimatedModel* ptr)->GetMorphWeight(*$(StringHash* phash)) } |]

-- | Return whether is the master (first) animated model.
animatedModelIsMaster :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m Bool
animatedModelIsMaster p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int)$(AnimatedModel* ptr)->IsMaster() } |]

-- | Set model attribute.
animatedModelSetModelAttr :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> ResourceRef -- ^ Value
  -> m ()
animatedModelSetModelAttr p rref = liftIO $ with rref $ \rref' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(AnimatedModel* ptr)->SetModelAttr(*$(ResourceRef* rref')) } |]

-- | Set bones' animation enabled attribute.
animatedModelSetBonesEnabledAttr :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v (Ptr Variant)) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> v (Ptr Variant) -- ^ vector of variant's values
  -> m ()
animatedModelSetBonesEnabledAttr p v = liftIO $ withForeignVector () v $ \(v' :: Ptr VectorVariant) -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(AnimatedModel* ptr)->SetBonesEnabledAttr(*$(VectorVariant* v')) } |]

-- | Set animation states attribute.
animatedModelSetAnimationStatesAttr :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v (Ptr Variant)) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> v (Ptr Variant) -- ^ vector of variant's values
  -> m ()
animatedModelSetAnimationStatesAttr p v = liftIO $ withForeignVector () v $ \(v' :: Ptr VectorVariant) -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(AnimatedModel* ptr)->SetAnimationStatesAttr(*$(VectorVariant* v')) } |]

-- | Set morphs attribute.
animatedModelSetMorphsAttr :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v Word8) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> v Word8 -- ^ values of attribute
  -> m ()
animatedModelSetMorphsAttr p v = liftIO $ withForeignVector () v $ \(v' :: Ptr PODVectorWord8) -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(AnimatedModel* ptr)->SetMorphsAttr(*$(PODVectorWord8* v')) } |]

-- | Return model attribute.
animatedModelGetModelAttr :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m ResourceRef
animatedModelGetModelAttr p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.block| ResourceRef* { 
    static ResourceRef ref = $(AnimatedModel* ptr)->GetModelAttr();
    return &ref;
    } |]

-- | Return bones' animation enabled attribute.
animatedModelGetBonesEnabledAttr :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v (Ptr Variant)) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m (v (Ptr Variant))
animatedModelGetBonesEnabledAttr p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.block| VectorVariant* { 
    static VectorVariant v = $(AnimatedModel* ptr)->GetBonesEnabledAttr();
    return &v;
    } |]

-- | Return animation states attribute.
animatedModelGetAnimationStatesAttr :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v (Ptr Variant)) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m (v (Ptr Variant))
animatedModelGetAnimationStatesAttr p = liftIO $ do 
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.block| VectorVariant* { 
    static VectorVariant v = $(AnimatedModel* ptr)->GetAnimationStatesAttr();
    return &v;
    } |]

-- | Return morphs attribute.
animatedModelGetMorphsAttr :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v Word8) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m (v Word8)
animatedModelGetMorphsAttr p = liftIO $ do 
  let ptr = parentPointer p 
  peekForeignVectorAs =<< [C.exp| const PODVectorWord8* { &$(AnimatedModel* ptr)->GetMorphsAttr() } |]

-- | Return per-geometry bone mappings.
animatedModelGetGeometryBoneMappings :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v1, ForeignElemConstr v1 (Ptr PODVectorWord), ForeignVectorRepresent v2, ForeignElemConstr v2 Word, Traversable v1) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m (v1 (v2 Word))
animatedModelGetGeometryBoneMappings p = liftIO $ do 
  let ptr = parentPointer p 
  vtemp <- peekForeignVectorAs =<< [C.exp| const VectorPODVectorWord* { &$(AnimatedModel* ptr)->GetGeometryBoneMappings() } |]
  mapM peekForeignVectorAs vtemp

-- | Return per-geometry skin matrices. If empty, uses global skinning
animatedModelGetGeometrySkinMatrices :: (Parent AnimatedModel a, Pointer p a, MonadIO m, ForeignVectorRepresent v1, ForeignElemConstr v1 (Ptr PODVectorMatrix3x4), ForeignVectorRepresent v2, ForeignElemConstr v2 Matrix3x4, Traversable v1) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m (v1 (v2 Matrix3x4))
animatedModelGetGeometrySkinMatrices p = liftIO $ do 
  let ptr = parentPointer p 
  vtemp <- peekForeignVectorAs =<< [C.exp| const VectorPODVectorMatrix3x4* { &$(AnimatedModel* ptr)->GetGeometrySkinMatrices() } |]
  mapM peekForeignVectorAs vtemp

-- | Recalculate the bone bounding box. Normally called internally, but can also be manually called if up-to-date information before rendering is necessary.
animatedModelUpdateBoneBoundingBox :: (Parent AnimatedModel a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to AnimatedModel or ascentor
  -> m ()
animatedModelUpdateBoneBoundingBox p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(AnimatedModel* ptr)->UpdateBoneBoundingBox() } |]