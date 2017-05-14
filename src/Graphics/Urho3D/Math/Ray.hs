{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Ray(
    Ray(..)
  , HasOrigin(..)
  , HasDirection(..)
  , rayContext
  , makeRay
  , rayProject
  , rayDistance
  , rayClosestPoint
  , RayHitDistance(..)
  , rayHitDistanceFrustum
  , rayHitDistanceTriangle
  , rayHitDistanceGeometry
  , rayHitDistanceIndexedGeometry
  , rayInsideGeometry
  , rayInsideIndexedGeometry
  , rayTransformed
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Math.Frustum
import Graphics.Urho3D.Math.Internal.Ray
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Math.Plane
import Graphics.Urho3D.Math.Sphere
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ

C.context (C.cppCtx
  <> rayCntx
  <> vector2Context
  <> vector3Context
  <> boundingBoxContext
  <> frustumContext
  <> planeContext
  <> sphereContext
  <> matrix3x4Context
  )
C.include "<Urho3D/Math/Ray.h>"
C.using "namespace Urho3D"

rayContext :: C.Context
rayContext = rayCntx

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

instance Storable Ray where
  sizeOf _ = fromIntegral [C.pure| int { (int)sizeof(Ray) } |]
  alignment _ = fromIntegral [C.pure| int { (int)Traits<Ray>::AlignmentOf } |]
  peek ptr = do
    _rayOrigin <- peek =<< [C.exp| Vector3* { &$(Ray* ptr)->origin_ } |]
    _rayDirection <- peek =<< [C.exp| Vector3* { &$(Ray* ptr)->direction_ } |]
    return Ray{..}
  poke ptr Ray{..} =
    with _rayOrigin $ \_rayOrigin' ->
    with _rayDirection $ \_rayDirection' ->
      [C.block| void {
        $(Ray* ptr)->origin_ = *$(Vector3* _rayOrigin');
        $(Ray* ptr)->direction_ = *$(Vector3* _rayDirection');
      } |]

-- | Construct from origin and direction. The direction will be normalized.
makeRay :: Vector3 -- ^ origin
  -> Vector3 -- ^ Direction
  -> Ray
makeRay o d = Ray o (vec3Normalize d)

-- | Project a point on the ray
rayProject :: Ray -- ^ ray to project on
  -> Vector3 -- ^ point
  -> Vector3 -- ^ projected point
rayProject (Ray origin_ direction_) point = origin_ + (vec3Dot offset direction_ `vec3Scale` direction_)
  where
    offset = point - origin_

-- | Return distance of a point from the ray.
rayDistance :: Ray -- ^ ray the distance calculated from
  -> Vector3 -- ^ point to calculate distance to
  -> Float
rayDistance r1 point = vec3Length $ point - projected
  where projected = rayProject r1 point

-- | Return closest point to another ray.
rayClosestPoint :: Ray
  -> Ray
  -> Vector3
rayClosestPoint r1 r2 = unsafePerformIO $ with r1 $ \r1' -> with r2 $ \r2' -> alloca $ \res -> do
  [C.block| void {
    *$(Vector3* res) = $(Ray* r1')->ClosestPoint(*$(Ray* r2'));
  } |]
  peek res
-- Vector3 ClosestPoint(const Ray& ray) const;

class RayHitDistance a where
  -- | Return hit distance to another object, or infinity if no hit
  rayHitDistance :: Ray
    -> a
    -> Float

-- | Return hit distance to a plane, or infinity if no hit.
instance RayHitDistance Plane where
  rayHitDistance r1 p = unsafePerformIO $ with r1 $ \r1' -> with p $ \p' -> realToFrac <$> [C.exp| float {
      $(Ray* r1')->HitDistance(*$(Plane* p'))
    } |]
  -- float HitDistance(const Plane& plane) const;

-- | Return hit distance to a bounding box, or infinity if no hit.
instance RayHitDistance BoundingBox  where
  rayHitDistance r1 b = unsafePerformIO $ with r1 $ \r1' -> with b $ \b' -> realToFrac <$> [C.exp| float {
      $(Ray* r1')->HitDistance(*$(BoundingBox* b'))
    } |]
    -- float HitDistance(const BoundingBox& box) const;

-- | Return hit distance to a frustum, or infinity if no hit.
instance RayHitDistance Frustum  where
  rayHitDistance r1 f = unsafePerformIO $ with r1 $ \r1' -> with f $ \f' -> realToFrac <$> [C.exp| float {
      $(Ray* r1')->HitDistance(*$(Frustum* f'))
    } |]
    -- float HitDistance(const Frustum& frustum, bool solidInside = true) const;

-- | Return hit distance to frustum, or infinity if no hit. If solidInside parameter is true (default) rays originating from inside return zero distance, otherwise the distance to the closest plane.
rayHitDistanceFrustum :: Ray
  -> Frustum
  -> Bool -- solid inside (default True)
  -> Float
rayHitDistanceFrustum r1 f b = unsafePerformIO $ with r1 $ \r1' -> with f $ \f' -> realToFrac <$> [C.exp| float {
    $(Ray* r1')->HitDistance(*$(Frustum* f'), $(int b') != 0)
  } |]
  where
    b' = fromBool b
    -- float HitDistance(const Frustum& frustum, bool solidInside = true) const;

-- | Return hit distance to a sphere, or infinity if no hit.
instance RayHitDistance Sphere  where
  rayHitDistance r1 f = unsafePerformIO $ with r1 $ \r1' -> with f $ \f' -> realToFrac <$> [C.exp| float {
      $(Ray* r1')->HitDistance(*$(Sphere* f'))
    } |]
    -- float HitDistance(const Sphere& sphere) const;

-- | Return hit distance to a triangle, or infinity if no hit. Optionally return hit normal and hit barycentric coordinate at intersect point.
rayHitDistanceTriangle :: Ray
  -> Vector3 -- First point
  -> Vector3 -- Second point
  -> Vector3 -- Third point
  -> (Float, Vector3, Vector3) -- ^ Return also normal and hit barycentric coordinate
rayHitDistanceTriangle r1 v1 v2 v3 = unsafePerformIO $ with r1 $ \r1' -> with v1 $ \v1' -> with v2 $ \v2' -> with v3 $ \v3' -> alloca $ \n' -> alloca $ \vb' -> do
  d <- realToFrac <$> [C.exp| float {
    $(Ray* r1')->HitDistance(*$(Vector3* v1'), *$(Vector3* v2'), *$(Vector3* v3'), $(Vector3* n'), $(Vector3* vb'))
  } |]
  (,,)
    <$> pure d
    <*> peek n'
    <*> peek vb'
-- float HitDistance(const Vector3& v0, const Vector3& v1, const Vector3& v2, Vector3* outNormal = 0, Vector3* outBary = 0) const;

-- | Return hit distance to a triangle, or infinity if no hit.
instance RayHitDistance (Vector3, Vector3, Vector3)  where
  rayHitDistance r1 (v1, v2, v3) = unsafePerformIO $ with r1 $ \r1' -> with v1 $ \v1' -> with v2 $ \v2' -> with v3 $ \v3' -> realToFrac <$> [C.exp| float {
      $(Ray* r1')->HitDistance(*$(Vector3* v1'), *$(Vector3* v2'), *$(Vector3* v3'))
    } |]
    -- float HitDistance(const Sphere& sphere) const;

-- | Return hit distance to non-indexed geometry data, or infinity if no hit. Optionally return hit normal and hit uv coordinates at intersect point.
rayHitDistanceGeometry :: Ray
  -> Ptr () -- vertex data
  -> Word -- vertex stride
  -> Word -- vertex start
  -> Word -- vertex count
  -> Word -- uv offset
  -> (Float, Vector3, Vector2) -- ^ Return also normal, UV coordinates of hit
rayHitDistanceGeometry r1 vdata vertexStride vertexStart vertexCount uvOffset = unsafePerformIO $ with r1 $ \r1' -> alloca $ \n' -> alloca $ \uv' -> do
  let vertexStride' = fromIntegral vertexStride
      vertexStart' = fromIntegral vertexStart
      vertexCount' = fromIntegral vertexCount
      uvOffset' = fromIntegral uvOffset
  d <- realToFrac <$> [C.exp| float {
    $(Ray* r1')->HitDistance($(void* vdata), $(unsigned int vertexStride'), $(unsigned int vertexStart'), $(unsigned int vertexCount'), $(Vector3* n'), $(Vector2* uv'), $(unsigned int uvOffset'))
  } |]
  (,,)
    <$> pure d
    <*> peek n'
    <*> peek uv'
-- float HitDistance
--     (const void* vertexData, unsigned vertexStride, unsigned vertexStart, unsigned vertexCount, Vector3* outNormal = 0,
--         Vector2* outUV = 0, unsigned uvOffset = 0) const;

-- | Return hit distance to indexed geometry data, or infinity if no hit. Optionally return hit normal and hit uv coordinates at intersect point.
rayHitDistanceIndexedGeometry :: Ray
  -> Ptr () -- vertex data
  -> Word -- vertex stride
  -> Ptr () -- index data
  -> Word -- index size
  -> Word -- index start
  -> Word -- index count
  -> Word -- uv offset
  -> (Float, Vector3, Vector2) -- ^ Return also normal, UV coordinates of hit
rayHitDistanceIndexedGeometry r1 vdata vertexStride idata indexSize indexStart indexCount uvOffset = unsafePerformIO $ with r1 $ \r1' -> alloca $ \n' -> alloca $ \uv' -> do
  let vertexStride' = fromIntegral vertexStride
      indexSize' = fromIntegral indexSize
      indexStart' = fromIntegral indexStart
      indexCount' = fromIntegral indexCount
      uvOffset' = fromIntegral uvOffset
  d <- realToFrac <$> [C.exp| float {
    $(Ray* r1')->HitDistance($(void* vdata), $(unsigned int vertexStride'), $(void* idata), $(unsigned int indexSize'), $(unsigned int indexStart')
      , $(unsigned int indexCount'), $(Vector3* n'), $(Vector2* uv'), $(unsigned int uvOffset'))
  } |]
  (,,)
    <$> pure d
    <*> peek n'
    <*> peek uv'
-- float HitDistance(const void* vertexData, unsigned vertexStride, const void* indexData, unsigned indexSize, unsigned indexStart,
--   unsigned indexCount, Vector3* outNormal = 0, Vector2* outUV = 0, unsigned uvOffset = 0) const;

-- | Return whether ray is inside non-indexed geometry.
rayInsideGeometry :: Ray
  -> Ptr () -- vertex data
  -> Word -- vertex size
  -> Word -- vertex start
  -> Word -- vertex count
  -> Bool
rayInsideGeometry r1 vdata vertexSize vertexStart vertexCount = unsafePerformIO $ with r1 $ \r1' -> do
  let vertexSize' = fromIntegral vertexSize
      vertexStart' = fromIntegral vertexStart
      vertexCount' = fromIntegral vertexCount
  toBool <$> [C.exp| int {
    $(Ray* r1')->InsideGeometry($(void* vdata), $(unsigned int vertexSize'), $(unsigned int vertexStart') , $(unsigned int vertexCount'))
  } |]
-- bool InsideGeometry(const void* vertexData, unsigned vertexSize, unsigned vertexStart, unsigned vertexCount) const;

-- | Return whether ray is inside indexed geometry.
rayInsideIndexedGeometry :: Ray
  -> Ptr () -- ^ vertex data
  -> Word -- ^ vertex size
  -> Ptr () -- ^ index data
  -> Word -- ^ index size
  -> Word -- ^ index start
  -> Word -- ^ index count
  -> Bool
rayInsideIndexedGeometry r1 vdata vertexSize idata indexSize indexStart indexCount = unsafePerformIO $ with r1 $ \r1' -> do
  let vertexSize' = fromIntegral vertexSize
      indexSize' = fromIntegral indexSize
      indexStart' = fromIntegral indexStart
      indexCount' = fromIntegral indexCount
  toBool <$> [C.exp| int {
    $(Ray* r1')->InsideGeometry($(void* vdata), $(unsigned int vertexSize'), $(void* idata), $(unsigned int indexSize'), $(unsigned int indexStart') , $(unsigned int indexCount'))
  } |]
-- bool InsideGeometry(const void* vertexData, unsigned vertexSize, const void* indexData, unsigned indexSize, unsigned indexStart,
--   unsigned indexCount) const;

-- | Return transformed by a 3x4 matrix. This may result in a non-normalized direction.
rayTransformed :: Ray
  -> Matrix3x4 -- ^ transform matrix
  -> Ray
rayTransformed r1 m = unsafePerformIO $ with r1 $ \r1' -> with m $ \m' -> alloca $ \res -> do
  [C.block| void {
    *$(Ray* res) = $(Ray* r1')->Transformed(*$(Matrix3x4* m'));
  } |]
  peek res
-- Ray Transformed(const Matrix3x4& transform) const;
