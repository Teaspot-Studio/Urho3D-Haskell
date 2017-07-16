{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.OctreeQuery(
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
  , octreeQueryContext
  , octreeQueryGetResult
  , octreeQueryGetDrawableFlags
  , octreeQueryGetViewMask
  , octreeQuerySetResult
  , octreeQuerySetDrawableFlags
  , octreeQuerySetViewMask
  , pointOctreeQueryGetPoint
  , pointOctreeQuerySetPoint
  , sphereOctreeQueryGetSphere
  , sphereOctreeQuerySetSphere
  , boxOctreeQueryGetBox
  , boxOctreeQuerySetBox
  , frustumOctreeQueryGetFrustum
  , frustumOctreeQuerySetFrustum
  , rayOctreeQueryGetResult
  , rayOctreeQuerySetResult
  , rayOctreeQueryGetRay
  , rayOctreeQuerySetRay
  , rayOctreeQueryGetDrawableFlags
  , rayOctreeQuerySetDrawableFlags
  , rayOctreeQueryGetViewMask
  , rayOctreeQuerySetViewMask
  , rayOctreeQueryGetMaxDistance
  , rayOctreeQuerySetMaxDistance
  , rayOctreeQueryGetLevel
  , rayOctreeQuerySetLevel
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Data.Vector (Vector)
import Data.Word
import Foreign
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Drawable
import Graphics.Urho3D.Graphics.Internal.OctreeQuery
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Math.Frustum
import Graphics.Urho3D.Math.Ray
import Graphics.Urho3D.Math.Sphere
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Node
import Text.RawString.QQ

C.context (C.cppCtx
  <> octreeQueryCntx
  <> boundingBoxContext
  <> drawableContext
  <> frustumContext
  <> nodeContext
  <> rayContext
  <> sphereContext
  <> vector2Context
  <> vector3Context
  )
C.include "<Urho3D/Graphics/OctreeQuery.h>"
C.using "namespace Urho3D"

C.verbatim "typedef PODVector<Drawable*> PODVectorDrawablePtr;"
C.verbatim "typedef PODVector<RayQueryResult> PODVectorRayQueryResult;"

simplePODVector "RayQueryResult"

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

octreeQueryContext :: C.Context
octreeQueryContext = octreeQueryCntx

-- | Get result vector
octreeQueryGetResult :: (Parent OctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> m (Vector (Ptr Drawable))
octreeQueryGetResult p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| PODVectorDrawablePtr* { &$(OctreeQuery* ptr)->result_ } |]

-- | Get drawable flags to include.
octreeQueryGetDrawableFlags :: (Parent OctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> m Word8
octreeQueryGetDrawableFlags p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned char { $(OctreeQuery* ptr)->drawableFlags_ } |]

-- | Get drawable layers to include.
octreeQueryGetViewMask :: (Parent OctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> m Word
octreeQueryGetViewMask p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned char { $(OctreeQuery* ptr)->viewMask_ } |]

-- | Set result vector
octreeQuerySetResult :: (Parent OctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> Vector (Ptr Drawable)
  -> m ()
octreeQuerySetResult p v = liftIO $ withForeignVector () v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(OctreeQuery* ptr)->result_ = *$(PODVectorDrawablePtr* v') } |]

-- | Set result vector
octreeQuerySetDrawableFlags :: (Parent OctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> Word8
  -> m ()
octreeQuerySetDrawableFlags p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(OctreeQuery* ptr)->drawableFlags_ = $(unsigned char v') } |]

-- | Set result vector
octreeQuerySetViewMask :: (Parent OctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> Word
  -> m ()
octreeQuerySetViewMask p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(OctreeQuery* ptr)->viewMask_ = $(unsigned int v') } |]

deriveParents [''OctreeQuery] ''AllContentOctreeQuery

-- | Takes drawableFlags and viewMask
instance Creatable (Ptr AllContentOctreeQuery) where
  type CreationOptions (Ptr AllContentOctreeQuery) = (Word8, Word)

  newObject (drawableFlags, viewMask) = liftIO $
    [C.exp| AllContentOctreeQuery* { new AllContentOctreeQuery(*(new PODVector<Drawable*>()), $(unsigned char drawableFlags'), $(unsigned int viewMask') ) } |]
    where
      drawableFlags' = fromIntegral drawableFlags
      viewMask' = fromIntegral viewMask
  deleteObject ptr = liftIO [C.block| void {
      delete &$(AllContentOctreeQuery* ptr)->result_;
      delete $(AllContentOctreeQuery* ptr);
    } |]

deriveParents [''OctreeQuery] ''PointOctreeQuery

-- | Takes point, drawableFlags and viewMask
instance Creatable (Ptr PointOctreeQuery) where
  type CreationOptions (Ptr PointOctreeQuery) = (Vector3, Word8, Word)

  newObject (point, drawableFlags, viewMask) = liftIO $ with point $ \point' ->
    [C.exp| PointOctreeQuery* { new PointOctreeQuery(*(new PODVector<Drawable*>()), *$(Vector3* point'), $(unsigned char drawableFlags'), $(unsigned int viewMask') ) } |]
    where
      drawableFlags' = fromIntegral drawableFlags
      viewMask' = fromIntegral viewMask
  deleteObject ptr = liftIO [C.block| void {
      delete &$(PointOctreeQuery* ptr)->result_;
      delete $(PointOctreeQuery* ptr);
    } |]

-- | Get query point
pointOctreeQueryGetPoint :: (Parent PointOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> m Vector3
pointOctreeQueryGetPoint p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| Vector3* { &$(PointOctreeQuery* ptr)->point_ } |]

-- | Set query point
pointOctreeQuerySetPoint :: (Parent PointOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> Vector3
  -> m ()
pointOctreeQuerySetPoint p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(PointOctreeQuery* ptr)->point_ = *$(Vector3* v') } |]

deriveParents [''OctreeQuery] ''SphereOctreeQuery

-- | Takes sphere, drawableFlags and viewMask
instance Creatable (Ptr SphereOctreeQuery) where
  type CreationOptions (Ptr SphereOctreeQuery) = (Sphere, Word8, Word)

  newObject (sphere, drawableFlags, viewMask) = liftIO $ with sphere $ \sphere' ->
    [C.exp| SphereOctreeQuery* { new SphereOctreeQuery(*(new PODVector<Drawable*>()), *$(Sphere* sphere'), $(unsigned char drawableFlags'), $(unsigned int viewMask') ) } |]
    where
      drawableFlags' = fromIntegral drawableFlags
      viewMask' = fromIntegral viewMask
  deleteObject ptr = liftIO [C.block| void {
      delete &$(SphereOctreeQuery* ptr)->result_;
      delete $(SphereOctreeQuery* ptr);
    } |]

-- | Get query sphere
sphereOctreeQueryGetSphere :: (Parent SphereOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> m Sphere
sphereOctreeQueryGetSphere p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| Sphere* { &$(SphereOctreeQuery* ptr)->sphere_ } |]

-- | Set query point
sphereOctreeQuerySetSphere :: (Parent SphereOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> Sphere
  -> m ()
sphereOctreeQuerySetSphere p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(SphereOctreeQuery* ptr)->sphere_ = *$(Sphere* v') } |]

deriveParents [''OctreeQuery] ''BoxOctreeQuery

-- | Takes box, drawableFlags and viewMask
instance Creatable (Ptr BoxOctreeQuery) where
  type CreationOptions (Ptr BoxOctreeQuery) = (BoundingBox, Word8, Word)

  newObject (box, drawableFlags, viewMask) = liftIO $ with box $ \box' ->
    [C.exp| BoxOctreeQuery* { new BoxOctreeQuery(*(new PODVector<Drawable*>()), *$(BoundingBox* box'), $(unsigned char drawableFlags'), $(unsigned int viewMask') ) } |]
    where
      drawableFlags' = fromIntegral drawableFlags
      viewMask' = fromIntegral viewMask
  deleteObject ptr = liftIO [C.block| void {
      delete &$(BoxOctreeQuery* ptr)->result_;
      delete $(BoxOctreeQuery* ptr);
    } |]

-- | Get query sphere
boxOctreeQueryGetBox :: (Parent BoxOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> m BoundingBox
boxOctreeQueryGetBox p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| BoundingBox* { &$(BoxOctreeQuery* ptr)->box_ } |]

-- | Set query point
boxOctreeQuerySetBox :: (Parent BoxOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> BoundingBox
  -> m ()
boxOctreeQuerySetBox p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(BoxOctreeQuery* ptr)->box_ = *$(BoundingBox* v') } |]

deriveParents [''OctreeQuery] ''FrustumOctreeQuery

-- | Takes frustum, drawableFlags and viewMask
instance Creatable (Ptr FrustumOctreeQuery) where
  type CreationOptions (Ptr FrustumOctreeQuery) = (Frustum, Word8, Word)

  newObject (box, drawableFlags, viewMask) = liftIO $ with box $ \box' ->
    [C.exp| FrustumOctreeQuery* { new FrustumOctreeQuery(*(new PODVector<Drawable*>()), *$(Frustum* box'), $(unsigned char drawableFlags'), $(unsigned int viewMask') ) } |]
    where
      drawableFlags' = fromIntegral drawableFlags
      viewMask' = fromIntegral viewMask
  deleteObject ptr = liftIO [C.block| void {
      delete &$(FrustumOctreeQuery* ptr)->result_;
      delete $(FrustumOctreeQuery* ptr);
    } |]

-- | Get query frustum
frustumOctreeQueryGetFrustum :: (Parent FrustumOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> m Frustum
frustumOctreeQueryGetFrustum p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| Frustum* { &$(FrustumOctreeQuery* ptr)->frustum_ } |]

-- | Set query frustum
frustumOctreeQuerySetFrustum :: (Parent FrustumOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'OctreeQuery' or ascenstor
  -> Frustum
  -> m ()
frustumOctreeQuerySetFrustum p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(FrustumOctreeQuery* ptr)->frustum_ = *$(Frustum* v') } |]

-- | Takes ray, query level, max distance, drawableFlags and viewMask
instance Creatable (Ptr RayOctreeQuery) where
  type CreationOptions (Ptr RayOctreeQuery) = (Ray, RayQueryLevel, Float, Word8, Word)

  newObject (ray, level, maxDistance, drawableFlags, viewMask) = liftIO $ with ray $ \ray' ->
    [C.exp| RayOctreeQuery* { new RayOctreeQuery(*(new PODVector<RayQueryResult>()), *$(Ray* ray'), (RayQueryLevel)$(int level'), $(float maxDistance'), $(unsigned char drawableFlags'), $(unsigned int viewMask') ) } |]
    where
      drawableFlags' = fromIntegral drawableFlags
      viewMask' = fromIntegral viewMask
      maxDistance' = realToFrac maxDistance
      level' = fromIntegral . fromEnum $ level
  deleteObject ptr = liftIO [C.block| void {
      delete &$(RayOctreeQuery* ptr)->result_;
      delete $(RayOctreeQuery* ptr);
    } |]

-- | Read results of ray octree query
rayOctreeQueryGetResult :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayQueryResult of ascentor
  -> m (Vector RayQueryResult)
rayOctreeQueryGetResult p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| PODVectorRayQueryResult* { &$(RayOctreeQuery* ptr)->result_ } |]

-- | Write results of ray octree query
rayOctreeQuerySetResult :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayQueryResult of ascentor
  -> Vector RayQueryResult
  -> m ()
rayOctreeQuerySetResult p v = liftIO $ withForeignVector () v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(RayOctreeQuery* ptr)->result_ = *$(PODVectorRayQueryResult* v') } |]

-- | Read ray of ray octree query
rayOctreeQueryGetRay :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> m Ray
rayOctreeQueryGetRay p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| Ray* { &$(RayOctreeQuery* ptr)->ray_ } |]

-- | Write ray of ray octree query
rayOctreeQuerySetRay :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> Ray
  -> m ()
rayOctreeQuerySetRay p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(RayOctreeQuery* ptr)->ray_ = *$(Ray* v') } |]

-- | Read drawable flags of ray octree query
rayOctreeQueryGetDrawableFlags :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> m Word8
rayOctreeQueryGetDrawableFlags p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned char { $(RayOctreeQuery* ptr)->drawableFlags_ } |]

-- | Write drawable flags of ray octree query
rayOctreeQuerySetDrawableFlags :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> Word8
  -> m ()
rayOctreeQuerySetDrawableFlags p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(RayOctreeQuery* ptr)->drawableFlags_ = $(unsigned char v') } |]

-- | Read view mask of ray octree query
rayOctreeQueryGetViewMask :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> m Word
rayOctreeQueryGetViewMask p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(RayOctreeQuery* ptr)->viewMask_ } |]

-- | Write view mask of ray octree query
rayOctreeQuerySetViewMask :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> Word
  -> m ()
rayOctreeQuerySetViewMask p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(RayOctreeQuery* ptr)->viewMask_ = $(unsigned int v') } |]

-- | Read maximum ray distance of ray octree query
rayOctreeQueryGetMaxDistance :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> m Float
rayOctreeQueryGetMaxDistance p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(RayOctreeQuery* ptr)->maxDistance_ } |]

-- | Write maximum ray distance of ray octree query
rayOctreeQuerySetMaxDistance :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> Float
  -> m ()
rayOctreeQuerySetMaxDistance p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(RayOctreeQuery* ptr)->maxDistance_ = $(float v') } |]

-- | Read raycast detail level of ray octree query
rayOctreeQueryGetLevel :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> m RayQueryLevel
rayOctreeQueryGetLevel p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int { (int)$(RayOctreeQuery* ptr)->level_ } |]

-- | Write raycast detail level of ray octree query
rayOctreeQuerySetLevel :: (Parent RayOctreeQuery a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to RayOctreeQuery of ascentor
  -> RayQueryLevel
  -> m ()
rayOctreeQuerySetLevel p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral . fromEnum $ v
  [C.exp| void { $(RayOctreeQuery* ptr)->level_ = (RayQueryLevel)$(int v') } |]

instance Storable RayQueryResult where
  sizeOf _ = fromIntegral [C.pure| int { (int)sizeof(RayQueryResult) } |]
  alignment _ = fromIntegral [C.pure| int { (int)Traits<RayQueryResult>::AlignmentOf } |]
  peek ptr = do
    _rayQueryResultPosition <- peek =<< [C.exp| Vector3* {&$(RayQueryResult* ptr)->position_ } |]
    _rayQueryResultNormal <- peek =<< [C.exp| Vector3* {&$(RayQueryResult* ptr)->normal_ } |]
    _rayQueryResultTextureUV <- peek =<< [C.exp| Vector2* {&$(RayQueryResult* ptr)->textureUV_ } |]
    _rayQueryResultDistance <- realToFrac <$> [C.exp| float {$(RayQueryResult* ptr)->distance_ } |]
    _rayQueryResultDrawable <- [C.exp| Drawable* {$(RayQueryResult* ptr)->drawable_ } |]
    _rayQueryResultNode <- [C.exp| Node* {$(RayQueryResult* ptr)->node_ } |]
    _rayQueryResultSubObject <- fromIntegral <$> [C.exp| unsigned int {$(RayQueryResult* ptr)->subObject_ } |]
    return RayQueryResult{..}
  poke ptr RayQueryResult{..} =
    with _rayQueryResultPosition $ \_rayQueryResultPosition' ->
    with _rayQueryResultNormal $ \_rayQueryResultNormal' ->
    with _rayQueryResultTextureUV $ \_rayQueryResultTextureUV' ->
      [C.block| void {
        $(RayQueryResult* ptr)->position_ = *$(Vector3* _rayQueryResultPosition');
        $(RayQueryResult* ptr)->normal_ = *$(Vector3* _rayQueryResultNormal');
        $(RayQueryResult* ptr)->textureUV_ = *$(Vector2* _rayQueryResultTextureUV');
        $(RayQueryResult* ptr)->distance_ = $(float _rayQueryResultDistance');
        $(RayQueryResult* ptr)->drawable_ = $(Drawable* _rayQueryResultDrawable);
        $(RayQueryResult* ptr)->node_ = $(Node* _rayQueryResultNode);
        $(RayQueryResult* ptr)->subObject_ = $(unsigned int _rayQueryResultSubObject');
      } |]
    where
      _rayQueryResultDistance' = realToFrac _rayQueryResultDistance
      _rayQueryResultSubObject' = fromIntegral _rayQueryResultSubObject
