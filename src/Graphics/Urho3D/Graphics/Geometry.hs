{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.Geometry(
    Geometry
  , SharedGeometry
  , VectorSharedPtrGeometry
  , VectorVectorSharedPtrGeometry
  , geometryContext
  , geometrySetNumVertexBuffers
  , geometrySetVertexBuffer
  , geometrySetIndexBuffer
  , geometrySetDrawRange
  , geometrySetDrawRangeWithVertex
  , geometrySetLodDistance
  , geometrySetRawVertexData
  , geometrySetRawVertexDataMask
  , geometrySetRawIndexData
  , geometryDraw
  , geometryGetVertexBuffers
  , geometryGetNumVertexBuffers
  , geometryGetVertexBuffer
  , geometryGetIndexBuffer
  , geometryGetPrimitiveType
  , geometryGetIndexStart
  , geometryGetIndexCount
  , geometryGetVertexStart
  , geometryGetVertexCount
  , geometryGetLodDistance
  , geometryGetBufferHash
  , geometryGetRawData
  , geometryGetRawDataShared
  , geometryGetHitDistance
  , geometryIsInside
  , geometryIsEmpty
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Control.Monad.IO.Class
import Data.Monoid
import Data.Vector (Vector)
import Foreign
import Graphics.Urho3D.Graphics.Internal.Geometry

import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Graphics.Graphics
import Graphics.Urho3D.Graphics.IndexBuffer
import Graphics.Urho3D.Graphics.VertexBuffer
import Graphics.Urho3D.Math.Ray
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent

C.context (C.cppCtx
  <> geometryCntx
  <> indexBufferContext
  <> objectContext
  <> sharedGeometryPtrCntx
  <> vertexBufferContext
  <> vector2Context
  <> vector3Context
  <> rayContext
  <> vectorContext
  <> graphicsContext
  <> contextContext
  )
C.include "<Urho3D/Graphics/Geometry.h>"
C.using "namespace Urho3D"

geometryContext :: C.Context
geometryContext = geometryCntx
  <> sharedGeometryPtrCntx

deriveParent ''Object ''Geometry

instance Creatable (Ptr Geometry) where
  type CreationOptions (Ptr Geometry) = Ptr Context

  newObject cntxPtr = liftIO $ [C.exp| Geometry* { new Geometry( $(Context* cntxPtr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Geometry* ptr) } |]

sharedPtr "Geometry"

C.verbatim "typedef SharedArrayPtr<unsigned char> SharedArrayWord8;"
C.verbatim "typedef PODVector<VertexElement> PODVectorVertexElement;"
C.verbatim "typedef Vector<SharedPtr<VertexBuffer> > VectorSharedPtrVertexBuffer;"
C.verbatim "typedef SharedArrayPtr<unsigned char> SharedArrayWord8;"

C.verbatim "typedef Vector< SharedPtr<Geometry> > VectorSharedPtrGeometry;"

instance Creatable (Ptr VectorSharedPtrGeometry) where
  type CreationOptions (Ptr VectorSharedPtrGeometry) = ()
  newObject _ = liftIO [C.exp| VectorSharedPtrGeometry* {new Vector<SharedPtr<Geometry> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorSharedPtrGeometry* ptr) } |]

instance ReadableVector VectorSharedPtrGeometry where
  type ReadVecElem VectorSharedPtrGeometry = SharedPtr Geometry
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorSharedPtrGeometry* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peekSharedPtr =<< [C.exp| SharedGeometry* { new SharedPtr<Geometry>((*$(VectorSharedPtrGeometry* ptr))[$(unsigned int i')]) } |]
    where i' = fromIntegral i

instance WriteableVector VectorSharedPtrGeometry where
  type WriteVecElem VectorSharedPtrGeometry = SharedPtr Geometry
  foreignVectorAppend ptr e = liftIO $ withSharedPtr e $ \e' -> [C.exp| void {$(VectorSharedPtrGeometry* ptr)->Push(*$(SharedGeometry* e')) } |]

C.verbatim "typedef Vector<Vector< SharedPtr<Geometry> > > VectorVectorSharedPtrGeometry;"

instance Creatable (Ptr VectorVectorSharedPtrGeometry) where
  type CreationOptions (Ptr VectorVectorSharedPtrGeometry) = ()
  newObject _ = liftIO [C.exp| VectorVectorSharedPtrGeometry* {new VectorVectorSharedPtrGeometry() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorVectorSharedPtrGeometry* ptr) } |]

instance ReadableVector VectorVectorSharedPtrGeometry where
  type ReadVecElem VectorVectorSharedPtrGeometry = Vector (SharedPtr Geometry)
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorVectorSharedPtrGeometry* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peekForeignVectorAs =<< [C.exp| VectorSharedPtrGeometry* { &(*$(VectorVectorSharedPtrGeometry* ptr))[$(unsigned int i')] } |]
    where i' = fromIntegral i

instance WriteableVector VectorVectorSharedPtrGeometry where
  type WriteVecElem VectorVectorSharedPtrGeometry = Vector (SharedPtr Geometry)
  foreignVectorAppend ptr v = liftIO $ withForeignVector () v $ \v' -> [C.exp| void {$(VectorVectorSharedPtrGeometry* ptr)->Push(*$(VectorSharedPtrGeometry* v')) } |]

-- | Set number of vertex buffers.
geometrySetNumVertexBuffers :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> Word -- ^ num
  -> m Bool
geometrySetNumVertexBuffers p n = liftIO $ do
  let ptr = parentPointer p
      n' = fromIntegral n
  toBool <$> [C.exp| int {$(Geometry* ptr)->SetNumVertexBuffers($(unsigned int n'))} |]
-- bool SetNumVertexBuffers(unsigned num);

-- | Set a vertex buffer by index.
geometrySetVertexBuffer :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> Word -- ^ index
  -> Ptr VertexBuffer -- ^ buffer
  -> m Bool
geometrySetVertexBuffer p i b = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  toBool <$> [C.exp| int {$(Geometry* ptr)->SetVertexBuffer($(unsigned int i'), $(VertexBuffer* b))} |]
-- bool SetVertexBuffer(unsigned index, VertexBuffer* buffer);

-- | Set the index buffer.
geometrySetIndexBuffer :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> Ptr IndexBuffer -- ^ buffer
  -> m ()
geometrySetIndexBuffer p b = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(Geometry* ptr)->SetIndexBuffer($(IndexBuffer* b))} |]
-- void SetIndexBuffer(IndexBuffer* buffer);

-- | Set the draw range.
geometrySetDrawRange :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> PrimitiveType -- ^ type
  -> Word -- ^ Index start
  -> Word -- ^ Index count
  -> Bool -- ^ Get used vertex range (default True)
  -> m Bool
geometrySetDrawRange p pt indexStart indexCount used = liftIO $ do
  let ptr = parentPointer p
      pt' = fromIntegral . fromEnum $ pt
      indexStart' = fromIntegral indexStart
      indexCount' = fromIntegral indexCount
      used' = fromBool used
  toBool <$> [C.exp| int {$(Geometry* ptr)->SetDrawRange((PrimitiveType)$(int pt'), $(unsigned int indexStart'), $(unsigned int indexCount'), $(int used') != 0)} |]
-- bool SetDrawRange(PrimitiveType type, unsigned indexStart, unsigned indexCount, bool getUsedVertexRange = true);

-- | Set the draw range.
geometrySetDrawRangeWithVertex :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> PrimitiveType -- ^ type
  -> Word -- ^ Index start
  -> Word -- ^ Index count
  -> Word -- ^ Vertex start
  -> Word -- ^ Vertex count
  -> Bool -- ^ Check illegal (default True)
  -> m Bool
geometrySetDrawRangeWithVertex p pt indexStart indexCount vertexStart vertexCount checkIllegal = liftIO $ do
  let ptr = parentPointer p
      pt' = fromIntegral . fromEnum $ pt
      indexStart' = fromIntegral indexStart
      indexCount' = fromIntegral indexCount
      vertexStart' = fromIntegral vertexStart
      vertexCount' = fromIntegral vertexCount
      checkIllegal' = fromBool checkIllegal
  toBool <$> [C.exp| int {$(Geometry* ptr)->SetDrawRange((PrimitiveType)$(int pt'), $(unsigned int indexStart'), $(unsigned int indexCount')
    , $(unsigned int vertexStart'), $(unsigned int vertexCount'), $(int checkIllegal') != 0)} |]
-- bool SetDrawRange(PrimitiveType type, unsigned indexStart, unsigned indexCount, unsigned vertexStart, unsigned vertexCount,
--    bool checkIllegal = true);

-- | Set the LOD distance.
geometrySetLodDistance :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> Float -- ^ distance
  -> m ()
geometrySetLodDistance p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void {$(Geometry* ptr)->SetLodDistance($(float d'))} |]
-- void SetLodDistance(float distance);

-- | Override raw vertex data to be returned for CPU-side operations.
geometrySetRawVertexData :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> SharedArrayPtr Word8 -- ^ data
  -> Vector VertexElement  -- ^ elements
  -> m ()
geometrySetRawVertexData p datum elements = liftIO $ withForeignVector () elements $ \elements' ->
  withSharedArrayPtr datum $ \_ datum' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Geometry* ptr)->SetRawVertexData(*$(SharedArrayWord8* datum'), *$(PODVectorVertexElement* elements'))} |]
-- void SetRawVertexData(SharedArrayPtr<unsigned char> data, const PODVector<VertexElement>& elements);

-- | Override raw vertex data to be returned for CPU-side operations using a legacy vertex bitmask.
geometrySetRawVertexDataMask :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> SharedArrayPtr Word8 -- ^ data
  -> Word -- ^ element mask
  -> m ()
geometrySetRawVertexDataMask p datum elementMask = liftIO $ withSharedArrayPtr datum $ \_ datum' -> do
  let ptr = parentPointer p
      elementMask' = fromIntegral elementMask
  [C.exp| void {$(Geometry* ptr)->SetRawVertexData(*$(SharedArrayWord8* datum'), $(unsigned int elementMask'))} |]
-- void SetRawVertexData(SharedArrayPtr<unsigned char> data, unsigned elementMask);

-- | Override raw index data to be returned for CPU-side operations.
geometrySetRawIndexData :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> SharedArrayPtr Word8 -- ^ data
  -> Word -- ^ index size
  -> m ()
geometrySetRawIndexData p datum indexSize = liftIO $ withSharedArrayPtr datum $ \_ datum' -> do
  let ptr = parentPointer p
      indexSize' = fromIntegral indexSize
  [C.exp| void {$(Geometry* ptr)->SetRawIndexData(*$(SharedArrayWord8* datum'), $(unsigned int indexSize'))} |]
-- void SetRawIndexData(SharedArrayPtr<unsigned char> data, unsigned indexSize);

-- | Draw.
geometryDraw :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> Ptr Graphics -- ^ graphics system
  -> m ()
geometryDraw p gr = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(Geometry* ptr)->Draw($(Graphics* gr))} |]
-- void Draw(Graphics* graphics);

-- | Return all vertex buffers.
geometryGetVertexBuffers :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m (Vector (SharedPtr VertexBuffer))
geometryGetVertexBuffers p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const VectorSharedPtrVertexBuffer* {&$(Geometry* ptr)->GetVertexBuffers()} |]
-- const Vector<SharedPtr<VertexBuffer> >& GetVertexBuffers() const { return vertexBuffers_; }

-- | Return number of vertex buffers.
geometryGetNumVertexBuffers :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m Word
geometryGetNumVertexBuffers p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Geometry* ptr)->GetNumVertexBuffers()} |]
-- unsigned GetNumVertexBuffers() const { return vertexBuffers_.Size(); }

-- | Return vertex buffer by index.
geometryGetVertexBuffer :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> Word -- ^ index
  -> m (Maybe (Ptr VertexBuffer))
geometryGetVertexBuffer p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  wrapNullPtr <$> [C.exp| VertexBuffer* {$(Geometry* ptr)->GetVertexBuffer($(unsigned int i'))} |]
-- VertexBuffer* GetVertexBuffer(unsigned index) const;

-- | Return the index buffer.
geometryGetIndexBuffer :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m (Maybe (Ptr IndexBuffer))
geometryGetIndexBuffer p = liftIO $ do
  let ptr = parentPointer p
  wrapNullPtr <$> [C.exp| IndexBuffer* {$(Geometry* ptr)->GetIndexBuffer()} |]
-- IndexBuffer* GetIndexBuffer() const { return indexBuffer_; }

-- | Return primitive type.
geometryGetPrimitiveType :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m PrimitiveType
geometryGetPrimitiveType p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(Geometry* ptr)->GetPrimitiveType()} |]
-- PrimitiveType GetPrimitiveType() const { return primitiveType_; }

-- | Return start index.
geometryGetIndexStart :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m Word
geometryGetIndexStart p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Geometry* ptr)->GetIndexStart()} |]
-- unsigned GetIndexStart() const { return indexStart_; }

-- | Return number of indices.
geometryGetIndexCount :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m Word
geometryGetIndexCount p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Geometry* ptr)->GetIndexCount()} |]
-- unsigned GetIndexCount() const { return indexCount_; }

-- | Return first used vertex.
geometryGetVertexStart :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m Word
geometryGetVertexStart p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Geometry* ptr)->GetVertexStart()} |]
-- unsigned GetVertexStart() const { return vertexStart_; }

-- | Return number of used vertices.
geometryGetVertexCount :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m Word
geometryGetVertexCount p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Geometry* ptr)->GetVertexCount()} |]
-- unsigned GetVertexCount() const { return vertexCount_; }

-- | Return LOD distance.
geometryGetLodDistance :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m Float
geometryGetLodDistance p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Geometry* ptr)->GetLodDistance()} |]
-- float GetLodDistance() const { return lodDistance_; }

-- | Return buffers' combined hash value for state sorting.
geometryGetBufferHash :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m Word
geometryGetBufferHash p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Geometry* ptr)->GetBufferHash()} |]
-- unsigned short GetBufferHash() const;

-- | Return raw vertex and index data for CPU operations, or null pointers if not available. Will return data of the first vertex buffer if override data not set.
geometryGetRawData :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m (
      Maybe (Ptr Word8) -- vertex data
    , Word -- vertex size
    , Maybe (Ptr Word8) -- index data
    , Word -- index size
    , Vector VertexElement -- elements
    ) -- ^ return (vertex data, vertex size, index data, index size, elements)
geometryGetRawData p = liftIO $ alloca $ \vertexData -> alloca $ \vertexSize -> alloca $ \indexData -> alloca $ \indexSize -> alloca $ \elements -> do
  let ptr = parentPointer p
  [C.block| void {
    const unsigned char *vertexData;
    unsigned vertexSize;
    const unsigned char *indexData;
    unsigned indexSize;
    const PODVectorVertexElement* elements;
    $(Geometry* ptr)->GetRawData(vertexData, vertexSize, indexData, indexSize, elements);
    *$(const unsigned char** vertexData) = vertexData;
    *$(unsigned int* vertexSize) = vertexSize;
    *$(const unsigned char** indexData) = indexData;
    *$(unsigned int* indexSize) = indexSize;
    *$(const PODVectorVertexElement** elements) = elements;
  } |]
  (,,,,)
    <$> (wrapNullPtr . castPtr <$> peek vertexData)
    <*> (fromIntegral <$> peek vertexSize)
    <*> (wrapNullPtr . castPtr <$> peek indexData)
    <*> fmap fromIntegral (peek indexSize)
    <*> (peekForeignVectorAs =<< peek elements)
-- void GetRawData(const unsigned char*& vertexData, unsigned& vertexSize, const unsigned char*& indexData, unsigned& indexSize, const PODVector<VertexElement>*& elements) const;

-- | Return raw vertex and index data for CPU operations, or null pointers if not available. Will return data of the first vertex buffer if override data not set.
geometryGetRawDataShared :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m (
      Maybe (SharedArrayPtr Word8) -- vertex data
    , Maybe (SharedArrayPtr Word8) -- index data
    , Vector VertexElement -- elements
    ) -- ^ return (vertex data, index data, elements)
geometryGetRawDataShared p = liftIO $ alloca $ \vertexData -> alloca $ \vertexSizePtr -> alloca $ \indexData -> alloca $ \indexSizePtr -> alloca $ \elements -> do
  let ptr = parentPointer p
  [C.block| void {
    SharedArrayPtr<unsigned char> vertexData;
    unsigned vertexSize;
    SharedArrayPtr<unsigned char> indexData;
    unsigned indexSize;
    const PODVectorVertexElement* elements;
    $(Geometry* ptr)->GetRawDataShared(vertexData, vertexSize, indexData, indexSize, elements);
    *$(SharedArrayWord8** vertexData) = new SharedArrayPtr<unsigned char>(vertexData);
    *$(unsigned int* vertexSizePtr) = vertexSize;
    *$(SharedArrayWord8** indexData) = new SharedArrayPtr<unsigned char>(indexData);
    *$(unsigned int* indexSizePtr) = indexSize;
    *$(const PODVectorVertexElement** elements) = elements;
  } |]
  vertexSize <- fromIntegral <$> peek vertexSizePtr
  indexSize <- fromIntegral <$> peek indexSizePtr
  (,,)
    <$> (fmap wrapNullPtr . peekSharedArrayPtr vertexSize =<< peek vertexData)
    <*> (fmap wrapNullPtr . peekSharedArrayPtr indexSize =<< peek indexData)
    <*> (peekForeignVectorAs =<< peek elements)
-- void GetRawDataShared(SharedArrayPtr<unsigned char>& vertexData, unsigned& vertexSize, SharedArrayPtr<unsigned char>& indexData,
--    unsigned& indexSize, const PODVector<VertexElement>*& elements) const;

-- | Return ray hit distance or infinity if no hit. Requires raw data to be set. Optionally return hit normal and hit uv coordinates at intersect point.
geometryGetHitDistance :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> Ray -- ^ ray
  -> m (Float, Vector3, Vector2)
geometryGetHitDistance p ray = liftIO $ with ray $ \ray' -> alloca $ \hv' -> alloca $ \uv' -> do
  let ptr = parentPointer p
  d <- realToFrac <$> [C.exp| float {$(Geometry* ptr)->GetHitDistance(*$(Ray* ray'), $(Vector3* hv'), $(Vector2* uv'))} |]
  (,,)
    <$> pure d
    <*> peek hv'
    <*> peek uv'
-- float GetHitDistance(const Ray& ray, Vector3* outNormal = 0, Vector2* outUV = 0) const;

-- | Return whether or not the ray is inside geometry.
geometryIsInside :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> Ray -- ^ ray
  -> m Bool
geometryIsInside p ray = liftIO $ with ray $ \ray' -> do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(Geometry* ptr)->IsInside(*$(Ray* ray'))} |]
-- bool IsInside(const Ray& ray) const;

-- | Return whether has empty draw range.
geometryIsEmpty :: (Parent Geometry a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Geometry or ascentor
  -> m Bool
geometryIsEmpty p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(Geometry* ptr)->IsEmpty()} |]
-- bool IsEmpty() const { return indexCount_ == 0 && vertexCount_ == 0; }
