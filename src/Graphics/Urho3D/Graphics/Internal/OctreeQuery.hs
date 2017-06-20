module Graphics.Urho3D.Graphics.Internal.OctreeQuery(
    OctreeQuery
  , PointOctreeQuery
  , SphereOctreeQuery
  , BoxOctreeQuery
  , FrustumOctreeQuery
  , RayOctreeQuery
  , AllContentOctreeQuery
  , RayQueryResult(..)
  , PODVectorRayQueryResult
  , RayQueryLevel(..)
  , HasPosition(..)
  , HasNormal(..)
  , HasTextureUV(..)
  , HasDistance(..)
  , HasDrawable(..)
  , HasNode(..)
  , HasSubObject(..)
  , octreeQueryCntx
  ) where

import Control.Lens
import Foreign
import GHC.Generics
import Graphics.Urho3D.Graphics.Internal.BillboardSet
import Graphics.Urho3D.Graphics.Internal.Drawable
import Graphics.Urho3D.Graphics.Internal.Skeleton
import Graphics.Urho3D.Math.Internal.Plane
import Graphics.Urho3D.Math.Internal.Vector2
import Graphics.Urho3D.Math.Internal.Vector3
import Graphics.Urho3D.Scene.Internal.Node
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

-- | Base class for octree queries.
data OctreeQuery
-- | Point octree query.
data PointOctreeQuery
-- | Sphere octree query.
data SphereOctreeQuery
-- | Bounding box octree query.
data BoxOctreeQuery
-- | Frustum octree query.
data FrustumOctreeQuery
-- | Raycast octree query.
data RayOctreeQuery
-- | Octree query that returns all scene.
data AllContentOctreeQuery
-- | Vector of RayQueryResult
data PODVectorRayQueryResult

-- | Raycast result.
data RayQueryResult = RayQueryResult {
  _rayQueryResultPosition :: {-# UNPACK #-} !Vector3 -- ^ Hit position in world space.
, _rayQueryResultNormal :: {-# UNPACK #-} !Vector3 -- ^ Hit normal in world space. Negation of ray direction if per-triangle data not available.
, _rayQueryResultTextureUV :: {-# UNPACK #-} !Vector2 -- ^ Hit texture position
, _rayQueryResultDistance :: {-# UNPACK #-} !Float -- ^ Distance from ray origin.
, _rayQueryResultDrawable :: {-# UNPACK #-} !(Ptr Drawable) -- ^ Drawable.
, _rayQueryResultNode :: {-# UNPACK #-} !(Ptr Node) -- ^ Scene node.
, _rayQueryResultSubObject :: {-# UNPACK #-} !Word -- ^ Drawable specific subobject if applicable.
} deriving (Eq, Show, Generic)

makeFields ''RayQueryResult

-- | Graphics raycast detail level.
data RayQueryLevel =
    RayAABB
  | RAYOBB
  | RayTriangle
  | RayTriangleUV
  deriving (Eq, Ord, Show, Read, Generic, Bounded, Enum)

octreeQueryCntx :: C.Context
octreeQueryCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "OctreeQuery", [t| OctreeQuery |])
    , (C.TypeName "PointOctreeQuery", [t| PointOctreeQuery |])
    , (C.TypeName "SphereOctreeQuery", [t| SphereOctreeQuery |])
    , (C.TypeName "BoxOctreeQuery", [t| BoxOctreeQuery |])
    , (C.TypeName "FrustumOctreeQuery", [t| FrustumOctreeQuery |])
    , (C.TypeName "RayOctreeQuery", [t| RayOctreeQuery |])
    , (C.TypeName "AllContentOctreeQuery", [t| AllContentOctreeQuery |])
    , (C.TypeName "RayQueryResult", [t| RayQueryResult |])
    , (C.TypeName "PODVectorRayQueryResult", [t|PODVectorRayQueryResult|])
    ]
  }
