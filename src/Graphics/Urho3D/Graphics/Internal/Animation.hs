module Graphics.Urho3D.Graphics.Internal.Animation(
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
  , AnimationTriggerPointImpl
  , HasTriggerData(..)
  , animationCntx
  , sharedAnimationPtrCntx
  , SharedAnimation
  , SharedAnimationPtr(..)
  , HashMapStringHashAnimationTrack
  , hashMapStringHashAnimationTrackCntx
  , VectorAnimationTriggerPoint
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr 
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Container.HashMap
import qualified Data.Map as Map

import Control.DeepSeq
import Control.Lens 
import GHC.Generics

import Graphics.Urho3D.Math.Internal.Vector3 
import Graphics.Urho3D.Math.Internal.Quaternion
import Graphics.Urho3D.Graphics.Internal.Skeleton
import qualified Data.Vector as V 
import Data.Word 

-- | Skeletal animation keyframe.
data AnimationKeyFrame = AnimationKeyFrame {
  _animationKeyFrameTime :: {-# UNPACK #-} !Float -- ^ Keyframe time.  
, _animationKeyFramePosition :: {-# UNPACK #-} !Vector3 -- ^ Bone position.
, _animationKeyFrameRotation :: {-# UNPACK #-} !Quaternion -- ^ Bone rotation.
, _animationKeyFrameScale :: {-# UNPACK #-} !Vector3 -- ^ Bone scale.
} deriving (Show, Eq, Generic)

makeFields ''AnimationKeyFrame
instance NFData AnimationKeyFrame

simpleVectorImpl "AnimationKeyFrame"

-- | Skeletal animation track, stores keyframes of a single bone.
data AnimationTrack = AnimationTrack {
  _animationTrackName :: !String -- ^ Bone or scene node name. 
, _animationTrackChannelMask :: {-# UNPACK #-} !Word8 -- ^ Bitmask of included data (position, rotation, scale.)
, _animationTrackKeyFrames :: !(V.Vector AnimationKeyFrame) -- ^ Keyframes.
} deriving (Show, Eq, Generic)

makeFields ''AnimationTrack
instance NFData AnimationTrack

-- | Animation trigger point.
data AnimationTriggerPoint a = AnimationTriggerPoint {
  _animationTriggerPointTime :: {-# UNPACK #-} !Float -- ^ Trigger time
, _animationTriggerPointTriggerData :: a -- ^ Trigger data
} deriving (Show, Eq, Generic)

makeFields ''AnimationTriggerPoint
instance NFData a => NFData (AnimationTriggerPoint a)

-- | Same as AnimationTriggerPoint but don't have type parameter
data AnimationTriggerPointImpl 

-- | Skeletal animation resource.
data Animation

-- | Vector<AnimationTriggerPoint>
data VectorAnimationTriggerPoint

animationCntx :: C.Context 
animationCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Animation", [t| Animation |])
    , (C.TypeName "AnimationKeyFrame", [t| AnimationKeyFrame |])
    , (C.TypeName "AnimationTrack", [t| AnimationTrack |])
    , (C.TypeName "AnimationTriggerPoint", [t| AnimationTriggerPointImpl |])
    , (C.TypeName "VectorAnimationTriggerPoint", [t| VectorAnimationTriggerPoint |])
    ]
  }

sharedPtrImpl "Animation"
hashMapImpl "StringHash" "AnimationTrack"