{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.AnimationState(
    AnimationState
  , SharedAnimationState
  , AnimationBlendMode(..)
  , animationStateContext
  , animationStateSetStartBone
  , animationStateSetLooped
  , animationStateSetWeight
  , animationStateSetBlendMode
  , animationStateSetTime
  , AnimationStateSetBoneWeight(..)
  , animationStateAddWeight
  , animationStateAddTime
  , animationStateSetLayer
  , animationStateGetAnimation
  , animationStateGetModel
  , animationStateGetNode
  , animationStateGetStartBone
  , AnimationStateGetBoneWeight(..)
  , AnimationStateGetTrackIndex(..)
  , animationStateIsEnabled
  , animationStateIsLooped
  , animationStateGetWeight
  , animationStateGetBlendMode
  , animationStateGetTime
  , animationStateGetLength
  , animationStateGetLayer
  , animationStateApply
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.AnimationState
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import Foreign.C.String 
import GHC.Generics
import Control.DeepSeq

import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Scene.Node

import Graphics.Urho3D.Graphics.Animation
import Graphics.Urho3D.Graphics.Internal.AnimatedModel
import Graphics.Urho3D.Graphics.Skeleton

C.context (C.cppCtx 
  <> animationStateCntx
  <> animatedModelCntx
  <> nodeContext
  <> skeletonContext
  <> animationContext 
  <> sharedAnimationStatePtrCntx)

C.include "<Urho3D/Scene/Node.h>"
C.include "<Urho3D/Math/StringHash.h>"
C.include "<Urho3D/Graphics/AnimationState.h>"
C.using "namespace Urho3D"

animationStateContext :: C.Context 
animationStateContext = animationStateCntx <> sharedAnimationStatePtrCntx

-- | Animation blending mode
data AnimationBlendMode = 
    AnimationBlendMode'Lerp  -- ^ Lerp blending (default)
  | AnimationBlendMode'Additive -- ^ Additive blending based on difference from bind pose 
  deriving (Eq, Show, Bounded, Enum, Generic)

instance NFData AnimationBlendMode 

-- | Construct with root scene node or animated model and animation pointers.
instance Createable (Ptr AnimationState) where 
  type CreationOptions (Ptr AnimationState) = (Either (Ptr AnimatedModel) (Ptr Node), Ptr Animation)

  newObject (Left pmodel, panim) = liftIO [C.exp| AnimationState* { new AnimationState($(AnimatedModel* pmodel), $(Animation* panim))} |]
  newObject (Right pnode, panim) = liftIO [C.exp| AnimationState* { new AnimationState($(Node* pnode), $(Animation* panim))} |]
  deleteObject ptr = liftIO [C.exp| void { delete $(AnimationState* ptr) } |]

sharedPtr "AnimationState"

C.verbatim "typedef Vector<SharedPtr<AnimationState> > VectorSharedAnimationStatePtr;"

instance Createable (Ptr VectorSharedAnimationStatePtr) where 
  type CreationOptions (Ptr VectorSharedAnimationStatePtr) = ()
  newObject _ = liftIO [C.exp| VectorSharedAnimationStatePtr* {new Vector<SharedPtr<AnimationState> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorSharedAnimationStatePtr* ptr) } |]

instance ReadableVector VectorSharedAnimationStatePtr where 
  type ReadVecElem VectorSharedAnimationStatePtr = SharedPtr AnimationState
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorSharedAnimationStatePtr* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peekSharedPtr =<< [C.exp| SharedAnimationState* { new SharedPtr<AnimationState>((*$(VectorSharedAnimationStatePtr* ptr))[$(unsigned int i')]) } |]
    where i' = fromIntegral i 

instance WriteableVector VectorSharedAnimationStatePtr where 
  type WriteVecElem VectorSharedAnimationStatePtr = SharedPtr AnimationState
  foreignVectorAppend ptr e = liftIO $ [C.exp| void {$(VectorSharedAnimationStatePtr* ptr)->Push(SharedPtr<AnimationState>($(AnimationState* e'))) } |]
    where e' = parentPointer e 

-- | Set start bone. Not supported in node animation mode. Resets any assigned per-bone weights.
animationStateSetStartBone :: (Parent AnimationState a, Pointer p a, MonadIO m, Parent Bone b, Pointer pbone b)
  => p -- ^ Pointer to AnimationState or ascentor
  -> pbone -- ^ Pointer to Bone or ascentor
  -> m ()
animationStateSetStartBone p pbone = liftIO $ do 
  let ptr = parentPointer p
      ptrbone = parentPointer pbone
  [C.exp| void { $(AnimationState* ptr)->SetStartBone($(Bone* ptrbone)) } |]

-- | Set looping enabled/disabled.
animationStateSetLooped :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> Bool -- ^ Looped?
  -> m ()
animationStateSetLooped p l = liftIO $ do 
  let ptr = parentPointer p
      l' = fromBool l
  [C.exp| void { $(AnimationState* ptr)->SetLooped($(int l') != 0) } |]

-- | Set blending weight.
animationStateSetWeight :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> Float -- ^ weight
  -> m ()
animationStateSetWeight p w = liftIO $ do 
  let ptr = parentPointer p
      w' = realToFrac w
  [C.exp| void { $(AnimationState* ptr)->SetWeight($(float w')) } |]

-- | Set blending mode.
animationStateSetBlendMode :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> AnimationBlendMode -- ^ mode
  -> m ()
animationStateSetBlendMode p m = liftIO $ do 
  let ptr = parentPointer p
      m' = fromIntegral . fromEnum $ m
  [C.exp| void { $(AnimationState* ptr)->SetBlendMode((AnimationBlendMode)$(int m')) } |]

-- | Set time position. Does not fire animation triggers.
animationStateSetTime :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> Float -- ^ time
  -> m ()
animationStateSetTime p t = liftIO $ do 
  let ptr = parentPointer p
      t' = realToFrac t 
  [C.exp| void { $(AnimationState* ptr)->SetTime($(float t')) } |]

class AnimationStateSetBoneWeight a where 
  -- | Set per-bone blending weight
  animationStateSetBoneWeight :: (Parent AnimationState b, Pointer p b, MonadIO m)
    => p -- ^ Pointer to AnimationState or ascentor
    -> a -- ^ Value to identify bone
    -> Float -- ^ weight
    -> Bool -- ^ recursive
    -> m ()

-- | Set per-bone blending weight by track index. Default is 1.0 (full), is multiplied  with the state's blending weight when applying the animation. Optionally recurses to child bones.
instance AnimationStateSetBoneWeight Int where 
  animationStateSetBoneWeight p i w r = liftIO $ do 
    let ptr = parentPointer p
        i' = fromIntegral i 
        w' = realToFrac w 
        r' = fromBool r 
    [C.exp| void { $(AnimationState* ptr)->SetBoneWeight($(unsigned int i'), $(float w'), $(int r') != 0) } |]

-- | Set per-bone blending weight by name.
instance AnimationStateSetBoneWeight String where 
  animationStateSetBoneWeight p n w r = liftIO $ withCString n $ \n' -> do 
    let ptr = parentPointer p
        w' = realToFrac w 
        r' = fromBool r 
    [C.exp| void { $(AnimationState* ptr)->SetBoneWeight(String($(const char* n')), $(float w'), $(int r') != 0) } |]

-- | Set per-bone blending weight by name hash.
instance AnimationStateSetBoneWeight (Ptr StringHash) where 
  animationStateSetBoneWeight p nhash w r = liftIO $ do 
    let ptr = parentPointer p
        w' = realToFrac w 
        r' = fromBool r 
    [C.exp| void { $(AnimationState* ptr)->SetBoneWeight(*$(StringHash* nhash), $(float w'), $(int r') != 0) } |]

-- | Modify blending weight.
animationStateAddWeight :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> Float -- ^ delta
  -> m ()
animationStateAddWeight p delta = liftIO $ do 
  let ptr = parentPointer p
      delta' = realToFrac delta
  [C.exp| void { $(AnimationState* ptr)->AddWeight($(float delta')) } |]

-- | Modify time position. %Animation triggers will be fired.
animationStateAddTime :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> Float
  -> m ()
animationStateAddTime p t = liftIO $ do 
  let ptr = parentPointer p
      t' = realToFrac t
  [C.exp| void { $(AnimationState* ptr)->AddTime($(float t')) } |]

-- | Set blending layer.
animationStateSetLayer :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> Word8 -- ^ layer
  -> m ()
animationStateSetLayer p l = liftIO $ do 
  let ptr = parentPointer p
      l' = fromIntegral l
  [C.exp| void { $(AnimationState* ptr)->SetLayer($(unsigned char l')) } |]

-- | Return animation.
animationStateGetAnimation :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m (Ptr Animation)
animationStateGetAnimation p = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| Animation* { $(AnimationState* ptr)->GetAnimation() } |]

-- | Return animated model this state belongs to (model mode.)
animationStateGetModel :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m (Ptr AnimatedModel)
animationStateGetModel p = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| AnimatedModel* { $(AnimationState* ptr)->GetModel() } |]

-- | Return root scene node this state controls (node hierarchy mode.)
animationStateGetNode :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m (Ptr Node)
animationStateGetNode p = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| Node* { $(AnimationState* ptr)->GetNode() } |]

-- | Return start bone.
animationStateGetStartBone :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m (Ptr Bone)
animationStateGetStartBone p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Bone* { $(AnimationState* ptr)->GetStartBone() } |]

class AnimationStateGetBoneWeight a where 
  -- | Return per-bone blending weight
  animationStateGetBoneWeight :: (Parent AnimationState b, Pointer p b, MonadIO m)
    => p -- ^ Pointer to AnimationState or ascentor
    -> a -- ^ Value to identify bone
    -> m Float 

-- | Return per-bone blending weight by track index.
instance AnimationStateGetBoneWeight Int where 
  animationStateGetBoneWeight p i = liftIO $ do 
    let ptr = parentPointer p
        i' = fromIntegral i 
    realToFrac <$> [C.exp| float { $(AnimationState* ptr)->GetBoneWeight($(unsigned int i')) } |]

-- | Return per-bone blending weight by name.
instance AnimationStateGetBoneWeight String where 
  animationStateGetBoneWeight p n = liftIO $ withCString n $ \n' -> do 
    let ptr = parentPointer p
    realToFrac <$> [C.exp| float { $(AnimationState* ptr)->GetBoneWeight(String($(const char* n'))) } |]

-- | Return per-bone blending weight by name hash.
instance AnimationStateGetBoneWeight (Ptr StringHash) where 
  animationStateGetBoneWeight p nhash = liftIO $ do 
    let ptr = parentPointer p
    realToFrac <$> [C.exp| float { $(AnimationState* ptr)->GetBoneWeight(*$(StringHash* nhash)) } |]

class AnimationStateGetTrackIndex a where 
  -- | Return per-bone blending weight
  animationStateGetTrackIndex :: (Parent AnimationState b, Pointer p b, MonadIO m)
    => p -- ^ Pointer to AnimationState or ascentor
    -> a -- ^ Value to identify bone
    -> m Int 

-- | Return per-bone blending weight by track index.
instance (Parent Node a, Pointer pnode a) => AnimationStateGetTrackIndex pnode where 
  animationStateGetTrackIndex p pnode = liftIO $ do 
    let ptr = parentPointer p
        ptrnode = parentPointer pnode
    fromIntegral <$> [C.exp| unsigned int { $(AnimationState* ptr)->GetTrackIndex($(Node* ptrnode)) } |]

-- | Return per-bone blending weight by name.
instance AnimationStateGetTrackIndex String where 
  animationStateGetTrackIndex p n = liftIO $ withCString n $ \n' -> do 
    let ptr = parentPointer p
    fromIntegral <$> [C.exp| unsigned int { $(AnimationState* ptr)->GetTrackIndex(String($(const char* n'))) } |]

-- | Return per-bone blending weight by name hash.
instance AnimationStateGetTrackIndex (Ptr StringHash) where 
  animationStateGetTrackIndex p nhash = liftIO $ do 
    let ptr = parentPointer p
    fromIntegral <$> [C.exp| unsigned int { $(AnimationState* ptr)->GetTrackIndex(*$(StringHash* nhash)) } |]

-- | Return whether weight is nonzero.
animationStateIsEnabled :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m Bool
animationStateIsEnabled p = liftIO $ do 
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(AnimationState* ptr)->IsEnabled() } |]

-- | Return whether looped.
animationStateIsLooped :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m Bool
animationStateIsLooped p = liftIO $ do 
  let ptr = parentPointer p
  toBool <$> [C.exp| int { $(AnimationState* ptr)->IsLooped() } |]

-- | Return blending weight.
animationStateGetWeight :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m Float
animationStateGetWeight p = liftIO $ do 
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { (int)$(AnimationState* ptr)->GetWeight() } |]

-- | Return blending mode.
animationStateGetBlendMode :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m AnimationBlendMode
animationStateGetBlendMode p = liftIO $ do 
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int { (int)$(AnimationState* ptr)->GetBlendMode() } |]

-- | Return time position.
animationStateGetTime :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m Float
animationStateGetTime p = liftIO $ do 
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(AnimationState* ptr)->GetTime() } |]

-- | Return animation length.
animationStateGetLength :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m Float
animationStateGetLength p = liftIO $ do 
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(AnimationState* ptr)->GetLength() } |]

-- | Return blending layer.
animationStateGetLayer :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m Word8
animationStateGetLayer p = liftIO $ do 
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned char { $(AnimationState* ptr)->GetLayer() } |]

-- | Apply the animation at the current time position.
animationStateApply :: (Parent AnimationState a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to AnimationState or ascentor
  -> m ()
animationStateApply p = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| void { $(AnimationState* ptr)->Apply() } |]
