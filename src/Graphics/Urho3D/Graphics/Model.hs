{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Model(
    Model
  , modelContext
  , modelSetBoundingBox
  , modelSetVertexBuffers
  , modelSetIndexBuffers
  , modelSetNumGeometries
  , modelSetNumGeometryLodLevels
  , modelSetGeometry
  , modelSetGeometryCenter
  , modelSetSkeleton
  , modelSetGeometryBoneMappings
  , modelSetMorphs
  , modelClone
  , modelGetBoundingBox
  , modelGetSkeleton
  , modelGetVertexBuffers
  , modelGetIndexBuffers
  , modelGetNumGeometries
  , modelGetNumGeometryLodLevels
  , modelGetGeometries
  , modelGetGeometryCenters
  , modelGetGeometry
  , modelGetGeometryCenter
  , modelGetGeometryBoneMappings
  , modelGetMorphs
  , modelGetNumMorphs
  , ModelGetMorph(..)
  , modelGetMorphRangeStart
  , modelGetMorphRangeCount
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Control.Monad.IO.Class
import Data.Monoid
import Data.Vector (Vector)
import Foreign
import Foreign.C.String
import Graphics.Urho3D.Graphics.Internal.Model

import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Geometry
import Graphics.Urho3D.Graphics.Skeleton
import Graphics.Urho3D.Graphics.ModelMorph
import Graphics.Urho3D.Graphics.VertexBuffer
import Graphics.Urho3D.Graphics.IndexBuffer
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx
  <> boundingBoxContext
  <> geometryContext
  <> indexBufferContext
  <> modelCntx
  <> modelMorphContext
  <> objectContext
  <> resourceContext
  <> sharedModelPtrCntx
  <> skeletonContext
  <> vector3Context
  <> vectorContext
  <> vertexBufferContext
  )

C.include "<Urho3D/Graphics/Model.h>"
C.using "namespace Urho3D"

C.verbatim "typedef Vector<SharedPtr<VertexBuffer> > VectorSharedPtrVertexBuffer;"
C.verbatim "typedef Vector<SharedPtr<IndexBuffer> > VectorSharedPtrIndexBuffer;"
C.verbatim "typedef PODVector<unsigned> PODVectorWord;"
C.verbatim "typedef Vector<PODVector<unsigned> > VectorPODVectorWord;"
C.verbatim "typedef Vector<ModelMorph> VectorModelMorph;"
C.verbatim "typedef Vector<Vector<SharedPtr<Geometry> > > VectorVectorSharedPtrGeometry;"
C.verbatim "typedef SharedPtr<Model> SharedPtrModel;"
C.verbatim "typedef PODVector<Vector3> PODVectorVector3;"

modelContext :: C.Context
modelContext = modelCntx <> resourceContext <> sharedModelPtrCntx

deriveParents [''Object, ''Resource] ''Model

instance ResourceType Model where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Model::GetTypeStatic().Value() } |]

sharedPtr "Model"

-- | Set local-space bounding box.
modelSetBoundingBox :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> BoundingBox
  -> m ()
modelSetBoundingBox p b = liftIO $ with b $ \b' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Model* ptr)->SetBoundingBox(*$(BoundingBox* b'))} |]

-- | Set vertex buffers and their morph ranges.
modelSetVertexBuffers :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Vector (SharedPtr VertexBuffer) -- ^ Buffers
  -> Vector Word -- ^ Morph range starts
  -> Vector Word -- ^ Morph range counts
  -> m Bool
modelSetVertexBuffers p buffers_ morphRangeStarts morphRangeCounts = liftIO $
  withForeignVector () buffers_ $ \buffers' ->
  withForeignVector () morphRangeStarts $ \morphRangeStarts' ->
  withForeignVector () morphRangeCounts $ \morphRangeCounts' -> do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(Model* ptr)->SetVertexBuffers(*$(VectorSharedPtrVertexBuffer* buffers')
    , *$(PODVectorWord* morphRangeStarts'), *$(PODVectorWord* morphRangeCounts'))} |]
-- bool SetVertexBuffers(const Vector<SharedPtr<VertexBuffer> >& buffers, const PODVector<unsigned>& morphRangeStarts,
--     const PODVector<unsigned>& morphRangeCounts);

-- | Set index buffers.
modelSetIndexBuffers :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Vector (SharedPtr IndexBuffer)
  -> m Bool
modelSetIndexBuffers p buffers_ = liftIO $ withForeignVector () buffers_ $ \buffers' -> do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(Model* ptr)->SetIndexBuffers(*$(VectorSharedPtrIndexBuffer* buffers'))} |]
-- bool SetIndexBuffers(const Vector<SharedPtr<IndexBuffer> >& buffers);

-- | Set number of geometries.
modelSetNumGeometries :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word
  -> m ()
modelSetNumGeometries p n = liftIO $ do
  let ptr = parentPointer p
      n' = fromIntegral n
  [C.exp| void {$(Model* ptr)->SetNumGeometries($(unsigned int n'))} |]
-- void SetNumGeometries(unsigned num);

-- | Set number of LOD levels in a geometry.
modelSetNumGeometryLodLevels :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ index
  -> Word -- ^ num
  -> m Bool
modelSetNumGeometryLodLevels p i n = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
      n' = fromIntegral n
  toBool <$> [C.exp| int {$(Model* ptr)->SetNumGeometryLodLevels($(unsigned int i'), $(unsigned int n'))} |]
-- bool SetNumGeometryLodLevels(unsigned index, unsigned num);

-- | Set geometry.
modelSetGeometry :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ index
  -> Word -- ^ LOD level
  -> Ptr Geometry
  -> m Bool
modelSetGeometry p i l pg = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
      l' = fromIntegral l
  toBool <$> [C.exp| int {$(Model* ptr)->SetGeometry($(unsigned int i'), $(unsigned int l'), $(Geometry* pg))} |]
-- bool SetGeometry(unsigned index, unsigned lodLevel, Geometry* geometry);

-- | Set geometry center.
modelSetGeometryCenter :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ index
  -> Vector3 -- ^ center
  -> m Bool
modelSetGeometryCenter p i v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
      i' = fromIntegral i
  toBool <$> [C.exp| int {$(Model* ptr)->SetGeometryCenter($(unsigned int i'), *$(Vector3* v'))} |]
-- bool SetGeometryCenter(unsigned index, const Vector3& center);

-- | Set skeleton.
modelSetSkeleton :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Ptr Skeleton
  -> m ()
modelSetSkeleton p s = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(Model* ptr)->SetSkeleton(*$(Skeleton* s))} |]
-- void SetSkeleton(const Skeleton& skeleton);

-- | Set bone mappings when model has more bones than the skinning shader can handle.
modelSetGeometryBoneMappings :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Vector (Vector Word) -- ^ mappings
  -> m ()
modelSetGeometryBoneMappings p mappings = liftIO $ withForeignVector () mappings $ \mappings' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Model* ptr)->SetGeometryBoneMappings(*$(VectorPODVectorWord* mappings'))} |]
-- void SetGeometryBoneMappings(const Vector<PODVector<unsigned> >& mappings);

-- | Set vertex morphs.
modelSetMorphs :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Vector ModelMorph -- ^ morphs
  -> m ()
modelSetMorphs p morphs = liftIO $ withForeignVector () morphs $ \morphs' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Model* ptr)->SetMorphs(*$(VectorModelMorph* morphs'))} |]
-- void SetMorphs(const Vector<ModelMorph>& morphs);

-- | Clone the model. The geometry data is deep-copied and can be modified in the clone without affecting the original.
modelClone :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> String -- ^ clone name (default empty)
  -> m (SharedPtr Model)
modelClone p nm = liftIO $ withCString nm $ \nm' -> do
  let ptr = parentPointer p
  peekSharedPtr =<< [C.exp| SharedModel* {new SharedPtrModel($(Model* ptr)->Clone(String($(const char* nm'))))} |]
-- SharedPtr<Model> Clone(const String& cloneName = String::EMPTY) const;

-- | Return bounding box.
modelGetBoundingBox :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m BoundingBox
modelGetBoundingBox p = liftIO $ alloca $ \b -> do
  let ptr = parentPointer p
  [C.exp| void { *$(BoundingBox* b) = $(Model* ptr)->GetBoundingBox() } |]
  peek b
-- const BoundingBox& GetBoundingBox() const { return boundingBox_; }

-- | Return skeleton.
modelGetSkeleton :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m (Ptr Skeleton)
modelGetSkeleton p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Skeleton* {&$(Model* ptr)->GetSkeleton()} |]
-- Skeleton& GetSkeleton() { return skeleton_; }

-- | Return vertex buffers.
modelGetVertexBuffers :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m (Vector (SharedPtr VertexBuffer))
modelGetVertexBuffers p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const VectorSharedPtrVertexBuffer* {&$(Model* ptr)->GetVertexBuffers()} |]
-- const Vector<SharedPtr<VertexBuffer> >& GetVertexBuffers() const { return vertexBuffers_; }

-- | Return index buffers.
modelGetIndexBuffers :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m (Vector (SharedPtr IndexBuffer))
modelGetIndexBuffers p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const VectorSharedPtrIndexBuffer* {&$(Model* ptr)->GetIndexBuffers()} |]
-- const Vector<SharedPtr<IndexBuffer> >& GetIndexBuffers() const { return indexBuffers_; }

-- | Return number of geometries.
modelGetNumGeometries :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m Word
modelGetNumGeometries p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Model* ptr)->GetNumGeometries()} |]
-- unsigned GetNumGeometries() const { return geometries_.Size(); }

-- | Return number of LOD levels in geometry.
modelGetNumGeometryLodLevels :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ index
  -> m Word
modelGetNumGeometryLodLevels p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  fromIntegral <$> [C.exp| unsigned int {$(Model* ptr)->GetNumGeometryLodLevels($(unsigned int i'))} |]
-- unsigned GetNumGeometryLodLevels(unsigned index) const;

-- | Return geometry pointers.
modelGetGeometries :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m (Vector (Vector (SharedPtr Geometry)))
modelGetGeometries p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const VectorVectorSharedPtrGeometry* {&$(Model* ptr)->GetGeometries()} |]
-- const Vector<Vector<SharedPtr<Geometry> > >& GetGeometries() const { return geometries_; }

-- | Return geometry center points.
modelGetGeometryCenters :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m (Vector Vector3)
modelGetGeometryCenters p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const PODVectorVector3* {&$(Model* ptr)->GetGeometryCenters()} |]
-- const PODVector<Vector3>& GetGeometryCenters() const { return geometryCenters_; }

-- | Return geometry by index and LOD level. The LOD level is clamped if out of range.
modelGetGeometry :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ index
  -> Word -- ^ LOD level
  -> m (Maybe (Ptr Geometry))
modelGetGeometry p i l = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
      l' = fromIntegral l
  wrapNullPtr <$> [C.exp| Geometry* {$(Model* ptr)->GetGeometry($(unsigned int i'), $(unsigned int l'))} |]
-- Geometry* GetGeometry(unsigned index, unsigned lodLevel) const;

-- | Return geometry center by index.
modelGetGeometryCenter :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ index
  -> m Vector3
modelGetGeometryCenter p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  peek =<< [C.exp| const Vector3* {&$(Model* ptr)->GetGeometryCenter($(unsigned int i'))} |]
-- const Vector3& GetGeometryCenter(unsigned index) const

-- | Return geometery bone mappings.
modelGetGeometryBoneMappings :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m (Vector (Vector Word))
modelGetGeometryBoneMappings p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const VectorPODVectorWord* {&$(Model* ptr)->GetGeometryBoneMappings()} |]
-- const Vector<PODVector<unsigned> >& GetGeometryBoneMappings() const { return geometryBoneMappings_; }

-- | Return vertex morphs.
modelGetMorphs :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m (Vector ModelMorph)
modelGetMorphs p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const VectorModelMorph* {&$(Model* ptr)->GetMorphs()} |]
-- const Vector<ModelMorph>& GetMorphs() const { return morphs_; }

-- | Return number of vertex morphs.
modelGetNumMorphs :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> m Word
modelGetNumMorphs p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Model* ptr)->GetNumMorphs()} |]
-- unsigned GetNumMorphs() const { return morphs_.Size(); }

class ModelGetMorph a where
  -- | Return vertex morph by index.
  modelGetMorph :: (Parent Model a, Pointer p a, MonadIO m)
    => p -- ^ Pointer to Animation or ascentor
    -> a
    -> m ModelMorph

  -- | Return vertex morph by index.
  modelSetMorph :: (Parent Model a, Pointer p a, MonadIO m)
    => p -- ^ Pointer to Animation or ascentor
    -> a
    -> ModelMorph
    -> m ()

instance ModelGetMorph Word where
  modelGetMorph p i = liftIO $ do
    let ptr = parentPointer p
        i' = fromIntegral i
    peek =<< [C.exp| const ModelMorph* {$(Model* ptr)->GetMorph($(unsigned int i'))} |]
  modelSetMorph p i m = liftIO $ do
    let ptr = parentPointer p
        i' = fromIntegral i
    m' <- [C.exp| const ModelMorph* {$(Model* ptr)->GetMorph($(unsigned int i'))} |]
    unless (isNull m') $ poke m' m
-- const ModelMorph* GetMorph(unsigned index) const;

-- | Return vertex morph by name.
instance ModelGetMorph String where
  modelGetMorph p nm = liftIO $ withCString nm $ \nm' -> do
    let ptr = parentPointer p
    peek =<< [C.exp| const ModelMorph* {$(Model* ptr)->GetMorph(String($(const char* nm')))} |]
  modelSetMorph p nm m = liftIO $ withCString nm $ \nm' -> do
    let ptr = parentPointer p
    m' <- [C.exp| const ModelMorph* {$(Model* ptr)->GetMorph(String($(const char* nm')))} |]
    unless (isNull m') $ poke m' m
-- const ModelMorph* GetMorph(const String& name) const;

-- | Return vertex morph by name hash.
instance ModelGetMorph StringHash where
  modelGetMorph p h = liftIO $ with h $ \h' -> do
    let ptr = parentPointer p
    peek =<< [C.exp| const ModelMorph* {$(Model* ptr)->GetMorph(*$(StringHash* h'))} |]
  modelSetMorph p h m = liftIO $ with h $ \h' -> do
    let ptr = parentPointer p
    m' <- [C.exp| const ModelMorph* {$(Model* ptr)->GetMorph(*$(StringHash* h'))} |]
    unless (isNull m') $ poke m' m
-- const ModelMorph* GetMorph(StringHash nameHash) const;

-- | Return vertex buffer morph range start.
modelGetMorphRangeStart :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ Buffer index
  -> m Word
modelGetMorphRangeStart p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  fromIntegral <$> [C.exp| unsigned int {$(Model* ptr)->GetMorphRangeStart($(unsigned int i'))} |]
-- unsigned GetMorphRangeStart(unsigned bufferIndex) const;

-- | Return vertex buffer morph range vertex count.
modelGetMorphRangeCount :: (Parent Model a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Animation or ascentor
  -> Word -- ^ Buffer index
  -> m Word
modelGetMorphRangeCount p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  fromIntegral <$> [C.exp| unsigned int {$(Model* ptr)->GetMorphRangeCount($(unsigned int i'))} |]
-- unsigned GetMorphRangeCount(unsigned bufferIndex) const;
