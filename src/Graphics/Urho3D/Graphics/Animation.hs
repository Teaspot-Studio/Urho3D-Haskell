{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Animation(
    Animation
  , AnimationKeyFrame(..)
  , HasTime(..)
  , HasPosition(..)
  , HasRotation(..)
  , HasScale(..)
  , VectorAnimationKeyFrame
  , vectorAnimationKeyFrameCntx
  , AnimationTrack(..)
  , HasName(..)
  , HasChannelMask(..)
  , HasKeyFrames(..)
  , AnimationTriggerPoint(..)
  , animationContext
  , SharedAnimation
  , HashMapStringHashAnimationTrack
  , channelPosition
  , channelRotation
  , channelScale
  , animationSetAnimationName
  , animationSetLength
  , animationCreateTrack
  , animationRemoveTrack
  , animationRemoveAllTracks
  , animationSetTrigger
  , animationAddTrigger
  , animationAddTrigger'
  , animationRemoveTrigger
  , animationRemoveAllTriggers
  , animationSetNumTriggers
  , animationGetAnimationName
  , animationGetAnimationNameHash
  , animationGetLength
  , animationGetTracks
  , animationGetNumTracks
  , AnimationGetTrack(..)
  , animationGetTriggers
  , animationGetNumTriggers
  , animationGetTrigger
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Animation
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.HashMap
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Monad
import Data.Monoid
import Data.Typeable
import Foreign
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ

import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Math.Vector3 
import Graphics.Urho3D.Math.Quaternion
import Graphics.Urho3D.Math.StringHash
import qualified Data.HashMap.Strict as H 
import qualified Data.Vector as V 
import Data.Maybe 

C.context (C.cppCtx <> C.funConstCtx
  <> sharedAnimationPtrCntx 
  <> vectorAnimationKeyFrameCntx
  <> hashMapStringHashAnimationTrackCntx
  <> animationCntx 
  <> resourceContext 
  <> objectContext
  <> contextContext
  <> vector3Context
  <> quaternionContext
  <> stringHashContext
  <> variantContext)

C.include "<Urho3D/Graphics/Animation.h>"
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

C.verbatim "typedef Vector<AnimationKeyFrame> VectorAnimationKeyFrame;"

simpleVector "AnimationKeyFrame"
hashMapPOD "StringHash" "AnimationTrack"

animationContext :: C.Context 
animationContext = animationCntx <> resourceContext <> sharedAnimationPtrCntx <> hashMapStringHashAnimationTrackCntx <> vectorAnimationKeyFrameCntx

instance Createable (Ptr Animation) where 
  type CreationOptions (Ptr Animation) = Ptr Context 

  newObject ptr = liftIO [C.exp| Animation* { new Animation($(Context* ptr)) } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(Animation* ptr) } |]

sharedPtr "Animation"
deriveParents [''Object, ''Resource] ''Animation

instance ResourceType Animation where 
  resourceType _ = unsafePerformIO $ [C.block| StringHash* { 
    static StringHash h = Animation::GetTypeStatic(); 
    return &h; 
    } |]

instance Storable AnimationKeyFrame where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(AnimationKeyFrame) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<AnimationKeyFrame>::AlignmentOf } |]
  peek ptr = do 
    _animationKeyFrameTime  <- realToFrac <$> [C.exp| float {$(AnimationKeyFrame* ptr)->time_} |]
    _animationKeyFramePosition <- peek =<< [C.exp| Vector3* {&$(AnimationKeyFrame* ptr)->position_} |]
    _animationKeyFrameRotation <- peek =<< [C.exp| Quaternion* {&$(AnimationKeyFrame* ptr)->rotation_} |]
    _animationKeyFrameScale <- peek =<< [C.exp| Vector3* {&$(AnimationKeyFrame* ptr)->scale_} |]
    return AnimationKeyFrame {..}
  poke ptr (AnimationKeyFrame {..}) = 
    with _animationKeyFramePosition $ \_animationKeyFramePosition' ->
    with _animationKeyFrameRotation $ \_animationKeyFrameRotation' ->
    with _animationKeyFrameScale $ \_animationKeyFrameScale' ->
      [C.block| void { 
        $(AnimationKeyFrame* ptr)->time_ = $(float _animationKeyFrameTime');
        $(AnimationKeyFrame* ptr)->position_ = *$(Vector3* _animationKeyFramePosition');
        $(AnimationKeyFrame* ptr)->rotation_ = *$(Quaternion* _animationKeyFrameRotation');
        $(AnimationKeyFrame* ptr)->scale_ = *$(Vector3* _animationKeyFrameScale');
      } |]
    where
    _animationKeyFrameTime' = realToFrac _animationKeyFrameTime

instance Storable AnimationTrack where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(AnimationTrack) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<AnimationTrack>::AlignmentOf } |]
  peek ptr = do 
    _animationTrackName <- peekCString =<< [C.exp| const char* {$(AnimationTrack* ptr)->name_.CString()} |]
    _animationTrackChannelMask <- fromIntegral <$> [C.exp| unsigned char {$(AnimationTrack* ptr)->channelMask_} |]
    _animationTrackKeyFrames <- peekForeignVectorAs =<< [C.exp| VectorAnimationKeyFrame* {&$(AnimationTrack* ptr)->keyFrames_} |]
    return AnimationTrack {..}
  poke ptr (AnimationTrack {..}) = 
    withCString _animationTrackName $ \_animationTrackName' -> 
      withObject _animationTrackName $ \_animationTrackHashName' ->
        withForeignVector () _animationTrackKeyFrames $ \_animationTrackKeyFrames' ->
        [C.block| void { 
          $(AnimationTrack* ptr)->name_ = String($(const char* _animationTrackName'));
          $(AnimationTrack* ptr)->nameHash_ = *$(StringHash* _animationTrackHashName');
          $(AnimationTrack* ptr)->channelMask_ = $(unsigned char _animationTrackChannelMask');
          $(AnimationTrack* ptr)->keyFrames_ = VectorAnimationKeyFrame(*$(VectorAnimationKeyFrame* _animationTrackKeyFrames'));
        } |]
    where 
    _animationTrackChannelMask' = fromIntegral _animationTrackChannelMask

instance (Typeable a, VariantStorable a) => Storable (AnimationTriggerPoint a) where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(AnimationTriggerPoint) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<AnimationTriggerPoint>::AlignmentOf } |]
  peek ptr = do 
    let ptr' = castPtr ptr
    _animationTriggerPointTime <- realToFrac <$> [C.exp| float {$(AnimationTriggerPoint* ptr')->time_} |]
    _animationTriggerPointTriggerData <- getVariantOrError =<< [C.exp| Variant* {&$(AnimationTriggerPoint* ptr')->data_} |]
    return AnimationTriggerPoint {..}
  poke ptr (AnimationTriggerPoint {..}) = 
    withVariant _animationTriggerPointTriggerData $ \_animationTriggerPointTriggerData' ->
    [C.block| void { 
      $(AnimationTriggerPoint* ptr')->time_ = $(float _animationTriggerPointTime');
      $(AnimationTriggerPoint* ptr')->data_ = *$(Variant* _animationTriggerPointTriggerData');
    } |]
    where
    _animationTriggerPointTime' = realToFrac _animationTriggerPointTime
    ptr' = castPtr ptr

C.verbatim "typedef Vector<AnimationTriggerPoint> VectorAnimationTriggerPoint;"

instance Createable (Ptr VectorAnimationTriggerPoint) where 
  type CreationOptions (Ptr VectorAnimationTriggerPoint) = ()
  newObject _ = liftIO [C.exp| VectorAnimationTriggerPoint* {new VectorAnimationTriggerPoint() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorAnimationTriggerPoint* ptr) } |]

instance ReadableVector VectorAnimationTriggerPoint where 
  type ReadVecElem VectorAnimationTriggerPoint = Ptr AnimationTriggerPointImpl
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorAnimationTriggerPoint* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ [C.exp| AnimationTriggerPoint* { &((*$(VectorAnimationTriggerPoint* ptr))[$(unsigned int i')]) } |]
    where i' = fromIntegral i 

instance WriteableVector VectorAnimationTriggerPoint where 
  type WriteVecElem VectorAnimationTriggerPoint = Ptr AnimationTriggerPointImpl
  foreignVectorAppend ptr e = liftIO $ [C.exp| void {$(VectorAnimationTriggerPoint* ptr)->Push(*$(AnimationTriggerPoint* e)) } |]

-- | Helper to avoid throwing bad typing in 'peek'
peekTriggerPoint :: (Typeable a, VariantStorable a, MonadIO m)
  => Ptr AnimationTriggerPointImpl 
  -> m (Maybe (AnimationTriggerPoint a))
peekTriggerPoint ptr = liftIO $ do 
  let ptr' = castPtr ptr
  _animationTriggerPointTime <- realToFrac <$> [C.exp| float {$(AnimationTriggerPoint* ptr')->time_} |]
  md <- getVariant =<< [C.exp| Variant* {&$(AnimationTriggerPoint* ptr')->data_} |]
  case md of 
    Nothing -> return Nothing 
    Just _animationTriggerPointTriggerData -> return $ Just AnimationTriggerPoint {..}

instance Functor AnimationTriggerPoint where 
  fmap f tr = tr { _animationTriggerPointTriggerData = f $ _animationTriggerPointTriggerData tr}

channelPosition :: Word8
channelPosition = fromIntegral [C.pure| unsigned char {CHANNEL_POSITION} |]

channelRotation :: Word8
channelRotation = fromIntegral [C.pure| unsigned char {CHANNEL_ROTATION} |]

channelScale :: Word8
channelScale = fromIntegral [C.pure| unsigned char {CHANNEL_SCALE} |]

-- | Set animation name.
animationSetAnimationName :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> String -- ^ name
  -> m ()
animationSetAnimationName p n = liftIO $ withCString n $ \n' -> do
  let ptr = parentPointer p 
  [C.exp| void {$(Animation* ptr)->SetAnimationName(String($(const char* n')))} |]

-- | Set animation length.
animationSetLength :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Float -- ^ length
  -> m ()
animationSetLength p l = liftIO $ do
  let ptr = parentPointer p 
      l' = realToFrac l
  [C.exp| void {$(Animation* ptr)->SetLength($(float l'))} |]

-- | Create and return a track by name. If track by same name already exists, returns the existing.
animationCreateTrack :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> String -- ^ name
  -> m (Ptr AnimationTrack)
animationCreateTrack p n = liftIO $ withCString n $ \n' -> do
  let ptr = parentPointer p 
  [C.exp| AnimationTrack* {$(Animation* ptr)->CreateTrack(String($(const char* n')))} |]

-- | Remove a track by name. Return true if was found and removed successfully. This is unsafe if the animation is currently used in playback.
animationRemoveTrack :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> String -- ^ name
  -> m ()
animationRemoveTrack p n = liftIO $ withCString n $ \n' -> do
  let ptr = parentPointer p 
  [C.exp| void {$(Animation* ptr)->RemoveTrack(String($(const char* n')))} |]

-- | Remove all tracks. This is unsafe if the animation is currently used in playback.
animationRemoveAllTracks :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m ()
animationRemoveAllTracks p = liftIO $ do
  let ptr = parentPointer p 
  [C.exp| void {$(Animation* ptr)->RemoveAllTracks()} |]

-- | Set a trigger point at index.
animationSetTrigger :: (Parent Animation a, Pointer p a, MonadIO m, Typeable b, VariantStorable b)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ index
  -> AnimationTriggerPoint b -- ^ trigger
  -> m ()
animationSetTrigger p i tp = liftIO $ with tp $ \tp' -> do
  let ptr = parentPointer p
      i' = fromIntegral i 
      tp'' = castPtr tp'
  [C.exp| void {$(Animation* ptr)->SetTrigger($(unsigned int i'), *$(AnimationTriggerPoint* tp''))} |]

-- | Add a trigger point.
animationAddTrigger :: (Parent Animation a, Pointer p a, MonadIO m, Typeable b, VariantStorable b)
  => p -- ^ Pointer to Animation or ascentor
  -> AnimationTriggerPoint b -- ^ trigger
  -> m ()
animationAddTrigger p tp = liftIO $ with tp $ \tp' -> do
  let ptr = parentPointer p 
      tp'' = castPtr tp'
  [C.exp| void {$(Animation* ptr)->AddTrigger(*$(AnimationTriggerPoint* tp''))} |]

-- | Add a trigger point.
animationAddTrigger' :: (Parent Animation a, Pointer p a, MonadIO m, VariantStorable b)
  => p -- ^ Pointer to Animation or ascentor
  -> Float -- ^ time 
  -> Bool -- ^ time is normalized
  -> b -- ^ data
  -> m ()
animationAddTrigger' p t n b = liftIO $ withVariant b $ \b' -> do
  let ptr = parentPointer p 
      t' = realToFrac t 
      n' = fromBool n
  [C.exp| void {$(Animation* ptr)->AddTrigger($(float t'), $(int n') != 0, *$(Variant* b'))} |]

-- | Remove a trigger point by index.
animationRemoveTrigger :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ index
  -> m ()
animationRemoveTrigger p i = liftIO $ do
  let ptr = parentPointer p 
      i' = fromIntegral i
  [C.exp| void {$(Animation* ptr)->RemoveTrigger($(unsigned int i'))} |]

-- | Remove all trigger points.
animationRemoveAllTriggers :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m ()
animationRemoveAllTriggers p = liftIO $ do
  let ptr = parentPointer p 
  [C.exp| void {$(Animation* ptr)->RemoveAllTriggers()} |]

-- | Resize trigger point vector.
animationSetNumTriggers :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ num
  -> m ()
animationSetNumTriggers p n = liftIO $ do
  let ptr = parentPointer p 
      n' = fromIntegral n
  [C.exp| void {$(Animation* ptr)->SetNumTriggers($(unsigned int n'))} |]

-- | Return animation name.
animationGetAnimationName :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m String
animationGetAnimationName p = liftIO $ do
  let ptr = parentPointer p 
  peekCString =<< [C.exp| const char* {$(Animation* ptr)->GetAnimationName().CString()} |]

-- | Return animation name hash.
animationGetAnimationNameHash :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m StringHash
animationGetAnimationNameHash p = liftIO $ do
  let ptr = parentPointer p 
  peek =<< [C.block| StringHash* {
    static StringHash h = $(Animation* ptr)->GetAnimationNameHash();
    return &h;
    } |]

-- | Return animation length.
animationGetLength :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m Float
animationGetLength p = liftIO $ do
  let ptr = parentPointer p 
  realToFrac <$> [C.exp| float {$(Animation* ptr)->GetLength()} |]

-- | Return all animation tracks.
animationGetTracks :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m (H.HashMap StringHash AnimationTrack)
animationGetTracks p = liftIO $ do
  let ptr = parentPointer p 
  peekForeignHashMap =<< [C.exp| const HashMapStringHashAnimationTrack* {&$(Animation* ptr)->GetTracks()} |]

-- | Return number of animation tracks.
animationGetNumTracks :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m Word
animationGetNumTracks p = liftIO $ do
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| unsigned int {$(Animation* ptr)->GetNumTracks()} |]

class AnimationGetTrack a where 
  animationGetTrack :: (Parent Animation b, Pointer p b, MonadIO m)
    => p -- ^ Pointer to Animation or ascentor
    -> a -- ^ identification value
    -> m (Ptr AnimationTrack)

-- | Return animation track by name.
instance AnimationGetTrack String where 
  animationGetTrack p s = liftIO $ withCString s $ \s' -> do
    let ptr = parentPointer p 
    [C.exp| AnimationTrack* {$(Animation* ptr)->GetTrack(String($(const char* s')))} |]

-- | Return animation track by name hash.
instance AnimationGetTrack StringHash where 
  animationGetTrack p s = liftIO $ with s $ \s' -> do
    let ptr = parentPointer p 
    [C.exp| AnimationTrack* {$(Animation* ptr)->GetTrack(*$(StringHash* s'))} |]

-- | Return animation trigger points. Note: you need to have trigger points of single type, all bad typed values are filtered out.
animationGetTriggers :: (Parent Animation a, Pointer p a, MonadIO m, Typeable b, VariantStorable b)
  => p -- ^ Pointer to Animation or ascentor
  -> m (V.Vector (AnimationTriggerPoint b))
animationGetTriggers p = liftIO $ do
  let ptr = parentPointer p 
  vtemp <- peekForeignVectorAs =<< [C.exp| const VectorAnimationTriggerPoint* {&$(Animation* ptr)->GetTriggers()} |]
  catMaybesVec <$> V.mapM peekTriggerPoint vtemp
  where 
  catMaybesVec = V.map fromJust . V.filter isJust

-- | Return number of animation trigger points.
animationGetNumTriggers :: (Parent Animation a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m Word
animationGetNumTriggers p = liftIO $ do
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| unsigned int {$(Animation* ptr)->GetNumTriggers()} |]

-- | Return a trigger point by index.
animationGetTrigger :: (Parent Animation a, Pointer p a, MonadIO m, Typeable b, VariantStorable b)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ index
  -> m (Maybe (AnimationTriggerPoint b))
animationGetTrigger p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  tmp <- [C.exp| AnimationTriggerPoint* {$(Animation* ptr)->GetTrigger($(unsigned int i'))} |]
  join <$> checkNullPtr' tmp peekTriggerPoint

