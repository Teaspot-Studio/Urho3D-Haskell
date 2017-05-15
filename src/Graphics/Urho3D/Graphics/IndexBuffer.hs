{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.IndexBuffer(
    IndexBuffer
  , indexBufferContext
  , SharedIndexBuffer
  , VectorSharedPtrIndexBuffer
  , indexBufferSetShadowed
  , indexBufferSetSize
  , indexBufferSetData
  , indexBufferSetDataRange
  , indexBufferLock
  , indexBufferUnlock
  , indexBufferIsShadowed
  , indexBufferIsDynamic
  , indexBufferIsLocked
  , indexBufferGetIndexCount
  , indexBufferGetIndexSize
  , indexBufferGetUsedVertexRange
  , indexBufferGetShadowData
  , indexBufferGetShadowDataShared
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad

import Graphics.Urho3D.Graphics.Internal.IndexBuffer

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent

C.context (C.cppCtx
  <> contextContext
  <> indexBufferCntx
  <> objectContext
  <> sharedIndexBufferPtrCntx
  <> vectorContext
  )
C.include "<Urho3D/Core/Object.h>"
C.include "<Urho3D/Graphics/IndexBuffer.h>"
C.using "namespace Urho3D"

indexBufferContext :: C.Context
indexBufferContext = sharedIndexBufferPtrCntx <> indexBufferCntx <> objectContext

instance Creatable (Ptr IndexBuffer) where
  type CreationOptions (Ptr IndexBuffer) = Ptr Context

  newObject cntxPtr = liftIO $ [C.exp| IndexBuffer* { new IndexBuffer( $(Context* cntxPtr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(IndexBuffer* ptr) } |]

deriveParent ''Object ''IndexBuffer

sharedPtr "IndexBuffer"

C.verbatim "typedef SharedArrayPtr<unsigned char> SharedArrayWord8;"

C.verbatim "typedef Vector<SharedPtr<IndexBuffer> > VectorSharedPtrIndexBuffer;"

instance Creatable (Ptr VectorSharedPtrIndexBuffer) where
  type CreationOptions (Ptr VectorSharedPtrIndexBuffer) = ()
  newObject _ = liftIO [C.exp| VectorSharedPtrIndexBuffer* {new Vector<SharedPtr<IndexBuffer> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorSharedPtrIndexBuffer* ptr) } |]

instance ReadableVector VectorSharedPtrIndexBuffer where
  type ReadVecElem VectorSharedPtrIndexBuffer = SharedPtr IndexBuffer
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorSharedPtrIndexBuffer* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peekSharedPtr =<< [C.exp| SharedIndexBuffer* { new SharedPtr<IndexBuffer>((*$(VectorSharedPtrIndexBuffer* ptr))[$(unsigned int i')]) } |]
    where i' = fromIntegral i

instance WriteableVector VectorSharedPtrIndexBuffer where
  type WriteVecElem VectorSharedPtrIndexBuffer = SharedPtr IndexBuffer
  foreignVectorAppend ptr e = liftIO $ [C.exp| void {$(VectorSharedPtrIndexBuffer* ptr)->Push(SharedPtr<IndexBuffer>($(IndexBuffer* e'))) } |]
    where e' = parentPointer e

-- | Enable shadowing in CPU memory. Shadowing is forced on if the graphics subsystem does not exist.
indexBufferSetShadowed :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> Bool -- ^ enabled
  -> m ()
indexBufferSetShadowed p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void {$(IndexBuffer* ptr)->SetShadowed($(int b') != 0)} |]
-- void SetShadowed(bool enable);

-- | Set size and vertex elements and dynamic mode. Previous data will be lost.
indexBufferSetSize :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> Word -- ^ index count
  -> Bool -- ^ large indicies
  -> Bool -- ^ dynamic (default false)
  -> m Bool
indexBufferSetSize p indexCount largeIndices dynamic = liftIO $ do
  let ptr = parentPointer p
      indexCount' = fromIntegral indexCount
      largeIndices' = fromBool largeIndices
      dynamic' = fromBool dynamic
  toBool <$> [C.exp| int {$(IndexBuffer* ptr)->SetSize($(unsigned int indexCount'), $(int largeIndices') != 0, $(int dynamic') != 0)} |]
-- bool SetSize(unsigned indexCount, bool largeIndices, bool dynamic = false);

-- | Set all data in the buffer.
indexBufferSetData :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> Ptr () -- ^ Data
  -> m Bool
indexBufferSetData p datum = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(IndexBuffer* ptr)->SetData($(void* datum))} |]
-- bool SetData(const void* data);

-- | Set a data range in the buffer. Optionally discard data outside the range.
indexBufferSetDataRange :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> Ptr () -- ^ Data
  -> Word -- ^ start
  -> Word -- ^ count
  -> Bool -- ^ discard (default false)
  -> m Bool
indexBufferSetDataRange p datum start count discard = liftIO $ do
  let ptr = parentPointer p
      start' = fromIntegral start
      count' = fromIntegral count
      discard' = fromBool discard
  toBool <$> [C.exp| int {$(IndexBuffer* ptr)->SetDataRange($(void* datum), $(unsigned int start'), $(unsigned int count'), $(int discard') != 0)} |]
-- bool SetDataRange(const void* data, unsigned start, unsigned count, bool discard = false);

-- | Lock the buffer for write-only editing. Return data pointer if successful. Optionally discard data outside the range.
indexBufferLock :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> Word -- ^ start
  -> Word -- ^ count
  -> Bool -- ^ discard (default false)
  -> m (Maybe (Ptr ()))
indexBufferLock p start count discard = liftIO $ do
  let ptr = parentPointer p
      start' = fromIntegral start
      count' = fromIntegral count
      discard' = fromBool discard
  wrapNullPtr <$> [C.exp| void* {$(IndexBuffer* ptr)->Lock($(unsigned int start'), $(unsigned int count'), $(int discard') != 0)} |]
-- void* Lock(unsigned start, unsigned count, bool discard = false);

-- | Unlock the buffer and apply changes to the GPU buffer.
indexBufferUnlock :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> m ()
indexBufferUnlock p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(IndexBuffer* ptr)->Unlock()} |]
-- void Unlock();

-- | Return whether CPU memory shadowing is enabled.
indexBufferIsShadowed :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> m Bool
indexBufferIsShadowed p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(IndexBuffer* ptr)->IsShadowed()} |]
-- bool IsShadowed() const { return shadowed_; }

-- | Return whether is dynamic.
indexBufferIsDynamic :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> m Bool
indexBufferIsDynamic p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(IndexBuffer* ptr)->IsDynamic()} |]
-- bool IsDynamic() const { return dynamic_; }

-- | Return whether is currently locked.
indexBufferIsLocked :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> m Bool
indexBufferIsLocked p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(IndexBuffer* ptr)->IsLocked()} |]
-- bool IsLocked() const { return lockState_ != LOCK_NONE; }

-- | Return number of indices.
indexBufferGetIndexCount :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> m Word
indexBufferGetIndexCount p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(IndexBuffer* ptr)->GetIndexCount()} |]
-- unsigned GetIndexCount() const { return indexCount_; }

-- | Return index size in bytes.
indexBufferGetIndexSize :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> m Word
indexBufferGetIndexSize p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(IndexBuffer* ptr)->GetIndexSize()} |]
-- unsigned GetIndexSize() const { return indexSize_; }

-- | Return used vertex range from index range.
indexBufferGetUsedVertexRange :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> Word -- ^ start
  -> Word -- ^ count
  -> m (Bool, Word, Word) -- ^ start index and count
indexBufferGetUsedVertexRange p start count = liftIO $ alloca $ \minVertex' -> alloca $ \vertexCount' -> do
  let ptr = parentPointer p
      start' = fromIntegral start
      count' = fromIntegral count
  res <- toBool <$> [C.block| int {
    unsigned minVertex;
    unsigned vertexCount;
    bool res = $(IndexBuffer* ptr)->GetUsedVertexRange($(unsigned int start'), $(unsigned int count'), minVertex, vertexCount);
    *$(unsigned int* minVertex') = minVertex;
    *$(unsigned int* vertexCount') = vertexCount;
    return res;
    } |]
  (,,) <$> pure res
       <*> (fromIntegral <$> peek minVertex')
       <*> (fromIntegral <$> peek vertexCount')
-- bool GetUsedVertexRange(unsigned start, unsigned count, unsigned& minVertex, unsigned& vertexCount);

-- | Return CPU memory shadow data.
indexBufferGetShadowData :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> m (Maybe (Ptr Word8))
indexBufferGetShadowData p = liftIO $ do
  let ptr = parentPointer p
  wrapNullPtr . castPtr <$> [C.exp| unsigned char* {$(IndexBuffer* ptr)->GetShadowData()} |]
-- unsigned char* GetShadowData() const { return shadowData_.Get(); }

-- | Return shared array pointer to the CPU memory shadow data.
--
-- Note: that attached length is zero (TODO: if possible get length data)
indexBufferGetShadowDataShared :: (Parent IndexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to IndexBuffer or ascentor
  -> m (Maybe (SharedArrayPtr Word8))
indexBufferGetShadowDataShared p = liftIO $ do
  let ptr = parentPointer p
  fmap wrapNullPtr . peekSharedArrayPtr 0 =<< [C.exp| SharedArrayWord8* {new SharedArrayWord8($(IndexBuffer* ptr)->GetShadowDataShared())} |]
-- SharedArrayPtr<unsigned char> GetShadowDataShared() const { return shadowData_; }
