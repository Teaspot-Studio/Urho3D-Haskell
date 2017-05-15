{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.VertexBuffer(
    VertexBuffer
  , vertexBufferContext
  , SharedVertexBuffer
  , VectorSharedPtrVertexBuffer
  , vertexBufferSetShadowed
  , vertexBufferSetSize
  , vertexBufferSetSizeMask
  , vertexBufferSetData
  , vertexBufferSetDataRange
  , vertexBufferLock
  , vertexBufferUnlock
  , vertexBufferIsShadowed
  , vertexBufferIsDynamic
  , vertexBufferIsLocked
  , vertexBufferGetVertexCount
  , vertexBufferGetVertexSize
  , vertexBufferGetElements
  , vertexBufferGetElement
  , vertexBufferGetElementByType
  , vertexBufferHasElement
  , vertexBufferHasElementByType
  , vertexBufferGetElementOffset
  , vertexBufferGetElementOffsetByType
  , vertexBufferGetElementMask
  , vertexBufferGetShadowData
  , vertexBufferGetShadowDataShared
  , vertexBufferGetElementByTypeFromList
  , vertexBufferHasElementByTypeFromList
  , vertexBufferGetElementOffsetByTypeFromList
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Data.Vector (Vector)
import Foreign
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Monad

import Graphics.Urho3D.Graphics.Internal.VertexBuffer

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent

C.context (C.cppCtx
  <> sharedVertexBufferPtrCntx
  <> vertexBufferCntx
  <> contextContext
  <> objectContext
  <> vectorContext
  <> graphDefsContext
  )
C.include "<Urho3D/Core/Object.h>"
C.include "<Urho3D/Graphics/VertexBuffer.h>"
C.using "namespace Urho3D"

vertexBufferContext :: C.Context
vertexBufferContext = sharedVertexBufferPtrCntx <> vertexBufferCntx <> objectContext

instance Creatable (Ptr VertexBuffer) where
  type CreationOptions (Ptr VertexBuffer) = Ptr Context

  newObject cntxPtr = liftIO $ [C.exp| VertexBuffer* { new VertexBuffer( $(Context* cntxPtr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(VertexBuffer* ptr) } |]

deriveParent ''Object ''VertexBuffer

sharedPtr "VertexBuffer"

C.verbatim "typedef PODVector<VertexElement> PODVectorVertexElement;"
C.verbatim "typedef SharedArrayPtr<unsigned char> SharedArrayWord8;"

C.verbatim "typedef Vector<SharedPtr<VertexBuffer> > VectorSharedPtrVertexBuffer;"

instance Creatable (Ptr VectorSharedPtrVertexBuffer) where
  type CreationOptions (Ptr VectorSharedPtrVertexBuffer) = ()
  newObject _ = liftIO [C.exp| VectorSharedPtrVertexBuffer* {new Vector<SharedPtr<VertexBuffer> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorSharedPtrVertexBuffer* ptr) } |]

instance ReadableVector VectorSharedPtrVertexBuffer where
  type ReadVecElem VectorSharedPtrVertexBuffer = SharedPtr VertexBuffer
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorSharedPtrVertexBuffer* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peekSharedPtr =<< [C.exp| SharedVertexBuffer* { new SharedPtr<VertexBuffer>((*$(VectorSharedPtrVertexBuffer* ptr))[$(unsigned int i')]) } |]
    where i' = fromIntegral i

instance WriteableVector VectorSharedPtrVertexBuffer where
  type WriteVecElem VectorSharedPtrVertexBuffer = SharedPtr VertexBuffer
  foreignVectorAppend ptr e = liftIO $ [C.exp| void {$(VectorSharedPtrVertexBuffer* ptr)->Push(SharedPtr<VertexBuffer>($(VertexBuffer* e'))) } |]
    where e' = parentPointer e

-- | Enable shadowing in CPU memory. Shadowing is forced on if the graphics subsystem does not exist.
vertexBufferSetShadowed :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> Bool -- ^ enabled
  -> m ()
vertexBufferSetShadowed p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void {$(VertexBuffer* ptr)->SetShadowed($(int b') != 0)} |]
-- void SetShadowed(bool enable);

-- | Set size, vertex elements and dynamic mode. Previous data will be lost.
vertexBufferSetSize :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> Word -- ^ vertex count
  -> Vector VertexElement -- ^ elements
  -> Bool -- ^ dynamic (default false)
  -> m Bool
vertexBufferSetSize p vertexCount elements dynamic = liftIO $ withForeignVector () elements $ \elements' -> do
  let ptr = parentPointer p
      vertexCount' = fromIntegral vertexCount
      dynamic' = fromBool dynamic
  toBool <$> [C.exp| int {$(VertexBuffer* ptr)->SetSize($(unsigned int vertexCount'), *$(PODVectorVertexElement* elements'), $(int dynamic') != 0)} |]
-- bool SetSize(unsigned vertexCount, const PODVector<VertexElement>& elements, bool dynamic = false);

-- | Set size and vertex elements and dynamic mode using legacy element bitmask. Previous data will be lost.
vertexBufferSetSizeMask :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> Word -- ^ vertex count
  -> Word -- ^ element mask
  -> Bool -- ^ dynamic (default false)
  -> m Bool
vertexBufferSetSizeMask p vertexCount elements dynamic = liftIO $ do
  let ptr = parentPointer p
      vertexCount' = fromIntegral vertexCount
      dynamic' = fromBool dynamic
      elements' = fromIntegral elements
  toBool <$> [C.exp| int {$(VertexBuffer* ptr)->SetSize($(unsigned int vertexCount'), $(unsigned int elements'), $(int dynamic') != 0)} |]
-- bool SetSize(unsigned vertexCount, unsigned elementMask, bool dynamic = false);

-- | Set all data in the buffer.
vertexBufferSetData :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> Ptr () -- ^ Data
  -> m Bool
vertexBufferSetData p datum = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(VertexBuffer* ptr)->SetData($(void* datum))} |]
-- bool SetData(const void* data);

-- | Set a data range in the buffer. Optionally discard data outside the range.
vertexBufferSetDataRange :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> Ptr () -- ^ Data
  -> Word -- ^ start
  -> Word -- ^ count
  -> Bool -- ^ discard (default false)
  -> m Bool
vertexBufferSetDataRange p datum start count discard = liftIO $ do
  let ptr = parentPointer p
      start' = fromIntegral start
      count' = fromIntegral count
      discard' = fromBool discard
  toBool <$> [C.exp| int {$(VertexBuffer* ptr)->SetDataRange($(void* datum), $(unsigned int start'), $(unsigned int count'), $(int discard') != 0)} |]
-- bool SetDataRange(const void* data, unsigned start, unsigned count, bool discard = false);

-- | Lock the buffer for write-only editing. Return data pointer if successful. Optionally discard data outside the range.
vertexBufferLock :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> Word -- ^ start
  -> Word -- ^ count
  -> Bool -- ^ discard (default false)
  -> m (Maybe (Ptr ()))
vertexBufferLock p start count discard = liftIO $ do
  let ptr = parentPointer p
      start' = fromIntegral start
      count' = fromIntegral count
      discard' = fromBool discard
  wrapNullPtr <$> [C.exp| void* {$(VertexBuffer* ptr)->Lock($(unsigned int start'), $(unsigned int count'), $(int discard') != 0)} |]
-- void* Lock(unsigned start, unsigned count, bool discard = false);

-- | Unlock the buffer and apply changes to the GPU buffer.
vertexBufferUnlock :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m ()
vertexBufferUnlock p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(VertexBuffer* ptr)->Unlock()} |]
-- void Unlock();

-- | Return whether CPU memory shadowing is enabled.
vertexBufferIsShadowed :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m Bool
vertexBufferIsShadowed p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(VertexBuffer* ptr)->IsShadowed()} |]
-- bool IsShadowed() const { return shadowed_; }

-- | Return whether is dynamic.
vertexBufferIsDynamic :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m Bool
vertexBufferIsDynamic p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(VertexBuffer* ptr)->IsDynamic()} |]
-- bool IsDynamic() const { return dynamic_; }

-- | Return whether is currently locked.
vertexBufferIsLocked :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m Bool
vertexBufferIsLocked p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {$(VertexBuffer* ptr)->IsLocked()} |]
-- bool IsLocked() const { return lockState_ != LOCK_NONE; }

-- | Return number of vertices.
vertexBufferGetVertexCount :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m Word
vertexBufferGetVertexCount p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(VertexBuffer* ptr)->GetVertexCount()} |]
-- unsigned GetVertexCount() const { return vertexCount_; }

-- | Return vertex size in bytes.
vertexBufferGetVertexSize :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m Word
vertexBufferGetVertexSize p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(VertexBuffer* ptr)->GetVertexSize()} |]
-- unsigned GetVertexSize() const { return vertexSize_; }

-- | Return vertex elements.
vertexBufferGetElements :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m (Vector VertexElement)
vertexBufferGetElements p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const PODVectorVertexElement* {&$(VertexBuffer* ptr)->GetElements()} |]
-- const PODVector<VertexElement>& GetElements() const { return elements_; }

-- | Return vertex element, or null if does not exist.
vertexBufferGetElement :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> VertexElementSemantic -- ^ sematic
  -> Word8 -- ^ index (default 0)
  -> m (Maybe VertexElement)
vertexBufferGetElement p semantic_ index_ = liftIO $ do
  let ptr = parentPointer p
      semantic' = fromIntegral . fromEnum $ semantic_
      index' = fromIntegral index_
  ve <- [C.exp| const VertexElement* {$(VertexBuffer* ptr)->GetElement((VertexElementSemantic)$(int semantic'), $(unsigned char index'))} |]
  checkNullPtr' ve peek
-- const VertexElement* GetElement(VertexElementSemantic semantic, unsigned char index = 0) const;

-- | Return vertex element with specific type, or null if does not exist.
vertexBufferGetElementByType :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> VertexElementType -- ^ type
  -> VertexElementSemantic -- ^ sematic
  -> Word8 -- ^ index (default 0)
  -> m (Maybe VertexElement)
vertexBufferGetElementByType p type_ semantic_ index_ = liftIO $ do
  let ptr = parentPointer p
      type' = fromIntegral . fromEnum $ type_
      semantic' = fromIntegral . fromEnum $ semantic_
      index' = fromIntegral index_
  ve <- [C.exp| const VertexElement* {$(VertexBuffer* ptr)->GetElement((VertexElementType)$(int type'), (VertexElementSemantic)$(int semantic'), $(unsigned char index'))} |]
  checkNullPtr' ve peek
-- const VertexElement* GetElement(VertexElementType type, VertexElementSemantic semantic, unsigned char index = 0) const;

-- | Return whether has a specified element semantic.
vertexBufferHasElement :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> VertexElementSemantic -- ^ sematic
  -> Word8 -- ^ index (default 0)
  -> m Bool
vertexBufferHasElement p semantic_ index_ = liftIO $ do
  let ptr = parentPointer p
      semantic' = fromIntegral . fromEnum $ semantic_
      index' = fromIntegral index_
  toBool <$> [C.exp| int {$(VertexBuffer* ptr)->HasElement((VertexElementSemantic)$(int semantic'), $(unsigned char index'))} |]
-- bool HasElement(VertexElementSemantic semantic, unsigned char index = 0) const { return GetElement(semantic, index) != 0; }

-- | Return whether has an element semantic with specific type.
vertexBufferHasElementByType :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> VertexElementType -- ^ type
  -> VertexElementSemantic -- ^ sematic
  -> Word8 -- ^ index (default 0)
  -> m Bool
vertexBufferHasElementByType p type_ semantic_ index_ = liftIO $ do
  let ptr = parentPointer p
      type' = fromIntegral . fromEnum $ type_
      semantic' = fromIntegral . fromEnum $ semantic_
      index' = fromIntegral index_
  toBool <$> [C.exp| int {$(VertexBuffer* ptr)->HasElement((VertexElementType)$(int type'), (VertexElementSemantic)$(int semantic'), $(unsigned char index'))} |]
-- bool HasElement(VertexElementType type, VertexElementSemantic semantic, unsigned char index = 0) const { return GetElement(type, semantic, index) != 0; }

-- | Return offset of a element within vertex, or M_MAX_UNSIGNED if does not exist.
vertexBufferGetElementOffset :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> VertexElementSemantic -- ^ sematic
  -> Word8 -- ^ index (default 0)
  -> m Word
vertexBufferGetElementOffset p semantic_ index_ = liftIO $ do
  let ptr = parentPointer p
      semantic' = fromIntegral . fromEnum $ semantic_
      index' = fromIntegral index_
  fromIntegral <$> [C.exp| unsigned int {$(VertexBuffer* ptr)->GetElementOffset((VertexElementSemantic)$(int semantic'), $(unsigned char index'))} |]
-- unsigned GetElementOffset(VertexElementSemantic semantic, unsigned char index = 0) const { const VertexElement* element = GetElement(semantic, index); return element ? element->offset_ : M_MAX_UNSIGNED; }

-- | Return offset of a element with specific type within vertex, or M_MAX_UNSIGNED if element does not exist.
vertexBufferGetElementOffsetByType :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> VertexElementType -- ^ type
  -> VertexElementSemantic -- ^ sematic
  -> Word8 -- ^ index (default 0)
  -> m Word
vertexBufferGetElementOffsetByType p type_ semantic_ index_ = liftIO $ do
  let ptr = parentPointer p
      type' = fromIntegral . fromEnum $ type_
      semantic' = fromIntegral . fromEnum $ semantic_
      index' = fromIntegral index_
  fromIntegral <$> [C.exp| unsigned int {$(VertexBuffer* ptr)->GetElementOffset((VertexElementType)$(int type'), (VertexElementSemantic)$(int semantic'), $(unsigned char index'))} |]
-- unsigned GetElementOffset(VertexElementType type, VertexElementSemantic semantic, unsigned char index = 0) const { const VertexElement* element = GetElement(type, semantic, index); return element ? element->offset_ : M_MAX_UNSIGNED; }

-- | Return legacy vertex element mask. Note that both semantic and type must match the legacy element for a mask bit to be set.
vertexBufferGetElementMask :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m Word
vertexBufferGetElementMask p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(VertexBuffer* ptr)->GetElementMask()} |]
-- unsigned GetElementMask() const { return elementMask_; }

-- | Return CPU memory shadow data.
vertexBufferGetShadowData :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m (Maybe (Ptr Word8))
vertexBufferGetShadowData p = liftIO $ do
  let ptr = parentPointer p
  wrapNullPtr . castPtr <$> [C.exp| unsigned char* {$(VertexBuffer* ptr)->GetShadowData()} |]
-- unsigned char* GetShadowData() const { return shadowData_.Get(); }

-- | Return shared array pointer to the CPU memory shadow data.
--
-- Note: that attached length is zero (TODO: if possible get length data)
vertexBufferGetShadowDataShared :: (Parent VertexBuffer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to VertexBuffer or ascentor
  -> m (Maybe (SharedArrayPtr Word8))
vertexBufferGetShadowDataShared p = liftIO $ do
  let ptr = parentPointer p
  fmap wrapNullPtr . peekSharedArrayPtr 0 =<< [C.exp| SharedArrayWord8* {new SharedArrayWord8($(VertexBuffer* ptr)->GetShadowDataShared())} |]
-- SharedArrayPtr<unsigned char> GetShadowDataShared() const { return shadowData_; }

-- | Return buffer hash for building vertex declarations. Used internally.
-- unsigned long long GetBufferHash(unsigned streamIndex) { return elementHash_ << (streamIndex * 16); }

-- | Return element with specified type and semantic from a vertex element list, or null if does not exist.
vertexBufferGetElementByTypeFromList :: MonadIO m
  => Vector VertexElement -- ^ elements
  -> VertexElementType -- ^ type
  -> VertexElementSemantic -- ^ sematic
  -> Word8 -- ^ index (default 0)
  -> m (Maybe VertexElement)
vertexBufferGetElementByTypeFromList elements type_ semantic_ index_ = liftIO $ withForeignVector () elements $ \elements' -> do
  let type' = fromIntegral . fromEnum $ type_
      semantic' = fromIntegral . fromEnum $ semantic_
      index' = fromIntegral index_
  ptr <- [C.exp| const VertexElement* { VertexBuffer::GetElement(*$(PODVectorVertexElement* elements'), (VertexElementType)$(int type'), (VertexElementSemantic)$(int semantic'), $(unsigned char index'))} |]
  checkNullPtr' ptr peek
-- static const VertexElement* GetElement(const PODVector<VertexElement>& elements, VertexElementType type, VertexElementSemantic semantic, unsigned char index = 0);

-- | Return whether element list has a specified element type and semantic.
vertexBufferHasElementByTypeFromList :: MonadIO m
  => Vector VertexElement -- ^ elements
  -> VertexElementType -- ^ type
  -> VertexElementSemantic -- ^ sematic
  -> Word8 -- ^ index (default 0)
  -> m Bool
vertexBufferHasElementByTypeFromList elements type_ semantic_ index_ = liftIO $ withForeignVector () elements $ \elements' -> do
  let type' = fromIntegral . fromEnum $ type_
      semantic' = fromIntegral . fromEnum $ semantic_
      index' = fromIntegral index_
  toBool <$> [C.exp| int {VertexBuffer::HasElement(*$(PODVectorVertexElement* elements'), (VertexElementType)$(int type'), (VertexElementSemantic)$(int semantic'), $(unsigned char index'))} |]
-- static bool HasElement(const PODVector<VertexElement>& elements, VertexElementType type, VertexElementSemantic semantic, unsigned char index = 0);

-- | Return element offset for specified type and semantic from a vertex element list, or M_MAX_UNSIGNED if does not exist.
vertexBufferGetElementOffsetByTypeFromList :: MonadIO m
  => Vector VertexElement -- ^ elements
  -> VertexElementType -- ^ type
  -> VertexElementSemantic -- ^ sematic
  -> Word8 -- ^ index (default 0)
  -> m Word
vertexBufferGetElementOffsetByTypeFromList elements type_ semantic_ index_ = liftIO $ withForeignVector () elements $ \elements' -> do
  let type' = fromIntegral . fromEnum $ type_
      semantic' = fromIntegral . fromEnum $ semantic_
      index' = fromIntegral index_
  fromIntegral <$> [C.exp| unsigned int {VertexBuffer::GetElementOffset(*$(PODVectorVertexElement* elements'), (VertexElementType)$(int type'), (VertexElementSemantic)$(int semantic'), $(unsigned char index'))} |]
-- static unsigned GetElementOffset(const PODVector<VertexElement>& elements, VertexElementType type, VertexElementSemantic semantic, unsigned char index = 0);

-- | Return a vertex element list from a legacy element bitmask.
-- static PODVector<VertexElement> GetElements(unsigned elementMask);

-- | Return vertex size from an element list.
-- static unsigned GetVertexSize(const PODVector<VertexElement>& elements);

-- | Return vertex size for a legacy vertex element bitmask.
-- static unsigned GetVertexSize(unsigned elementMask);
