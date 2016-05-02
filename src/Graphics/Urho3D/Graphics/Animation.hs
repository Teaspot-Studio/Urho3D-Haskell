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
  , SharedAnimationPtr(..)
  , wrapSharedAnimationPtr
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Animation
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Container.ForeignVector
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

C.context (C.cppCtx 
  <> sharedAnimationPtrCntx 
  <> vectorAnimationKeyFrameCntx
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

animationContext :: C.Context 
animationContext = animationCntx <> resourceContext <> sharedAnimationPtrCntx

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

instance Functor AnimationTriggerPoint where 
  fmap f tr = tr { _animationTriggerPointTriggerData = f $ _animationTriggerPointTriggerData tr}