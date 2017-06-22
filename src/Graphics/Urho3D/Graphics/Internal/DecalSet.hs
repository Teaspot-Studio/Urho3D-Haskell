module Graphics.Urho3D.Graphics.Internal.DecalSet(
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
  , decalSetCntx
  ) where

import Control.Lens
import Data.Vector (Vector)
import Data.Word
import GHC.Generics
import Graphics.Urho3D.Graphics.Internal.Animation
import Graphics.Urho3D.Graphics.Internal.Skeleton
import Graphics.Urho3D.Math.Internal.BoundingBox
import Graphics.Urho3D.Math.Internal.Plane
import Graphics.Urho3D.Math.Internal.Vector2
import Graphics.Urho3D.Math.Internal.Vector3
import Graphics.Urho3D.Math.Internal.Vector4
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

-- | Decal renderer component.
data DecalSet
-- | PODVector<DecalVertex>
data PODVectorDecalVertex

-- | Decal vertex
data DecalVertex = DecalVertex {
  _decalVertexPosition :: {-# UNPACK #-} !Vector3 -- ^ Position.
, _decalVertexNormal   :: {-# UNPACK #-} !Vector3 -- ^ Normal
, _decalVertexTexCoord :: {-# UNPACK #-} !Vector2 -- ^ Texture coordinates
, _decalVertexTangent  :: {-# UNPACK #-} !Vector4 -- ^ Tangent
, _decalVertexBlendWeights0 :: {-# UNPACK #-} !Float -- ^ Blend weights [0]
, _decalVertexBlendWeights1 :: {-# UNPACK #-} !Float -- ^ Blend weights [1]
, _decalVertexBlendWeights2 :: {-# UNPACK #-} !Float -- ^ Blend weights [2]
, _decalVertexBlendWeights3 :: {-# UNPACK #-} !Float -- ^ Blend weights [3]
, _decalVertexBlendIndices0 :: {-# UNPACK #-} !Word8 -- ^ Blend Indices [0]
, _decalVertexBlendIndices1 :: {-# UNPACK #-} !Word8 -- ^ Blend Indices [1]
, _decalVertexBlendIndices2 :: {-# UNPACK #-} !Word8 -- ^ Blend Indices [2]
, _decalVertexBlendIndices3 :: {-# UNPACK #-} !Word8 -- ^ Blend Indices [3]
} deriving (Eq, Generic)

makeFields ''DecalVertex

-- | One decal in a decal set.
data Decal = Decal {
  _decalTimer       :: {-# UNPACK #-} !Float -- ^ Decal age timer
, _decalTimeToLive  :: {-# UNPACK #-} !Float -- ^ Maximum time to live in seconds (0 = infinite)
, _decalBoundingBox :: {-# UNPACK #-} !BoundingBox -- ^ Local-space bounding box
, _decalVertecies   :: {-# UNPACK #-} !(Vector DecalVertex) -- ^ Decal vetecies
, _decalIndices    :: {-# UNPACK #-} !(Vector Word16) -- ^ Decal Indices
} deriving (Eq, Generic)

makeFields ''Decal

decalSetCntx :: C.Context
decalSetCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "DecalSet", [t| DecalSet |])
    , (C.TypeName "DecalVertex", [t| DecalVertex |])
    , (C.TypeName "Decal", [t| Decal |])
    , (C.TypeName "PODVectorDecalVertex", [t| PODVectorDecalVertex |])
    ]
  }
