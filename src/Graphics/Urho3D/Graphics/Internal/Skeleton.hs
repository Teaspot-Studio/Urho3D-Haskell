module Graphics.Urho3D.Graphics.Internal.Skeleton(
    Skeleton
  , Bone(..)
  , skeletonCntx
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
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map
import GHC.Generics 
import Control.DeepSeq 
import Control.Lens 
import Data.Word 

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Math.Quaternion
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Math.Internal.Sphere
import Graphics.Urho3D.Scene.Internal.Node 

data Skeleton 

-- | Bone in a skeleton
data Bone = Bone {
  _boneName :: !String -- ^ Bone name 
, _boneParentIndex :: {-# UNPACK #-} !Int -- ^ Parent bone index
, _boneInitialPosition :: {-# UNPACK #-} !Vector3 -- ^ Reset position
, _boneInitialRotation :: {-# UNPACK #-} !Quaternion -- ^ Reset rotation
, _boneInitialScale :: {-# UNPACK #-} !Vector3 -- ^ Reset scale
, _boneOffsetMatrix :: {-# UNPACK #-} !Matrix3x4 -- ^ Offset matrix
, _boneAnimated :: !Bool -- ^ Animation enable flag
, _boneCollisionMask :: {-# UNPACK #-} !Word8 -- ^ Supported collision types
, _boneRadius :: {-# UNPACK #-} !Float -- ^ Radius
, _boneBoundingBox :: {-# UNPACK #-} !BoundingBox -- ^ Local-space bounding box
, _boneNode :: {-# UNPACK #-} !(WeakPtr Node) -- ^ Scene node
} deriving (Generic)

makeFields ''Bone

instance NFData Bone where 
  rnf Bone{..} = 
    _boneName `deepseq`
    _boneParentIndex `deepseq`
    _boneInitialPosition `deepseq`
    _boneInitialRotation `deepseq`
    _boneInitialScale `deepseq`
    _boneOffsetMatrix `deepseq`
    _boneAnimated `deepseq`
    _boneCollisionMask `deepseq`
    _boneRadius `deepseq`
    _boneBoundingBox `deepseq`
    _boneNode `seq` ()

skeletonCntx :: C.Context 
skeletonCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Skeleton", [t| Skeleton |])
    , (C.TypeName "Bone", [t| Bone |])
    ]
  } 
