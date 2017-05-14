{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.IndexBuffer(
    IndexBuffer
  , indexBufferContext
  , SharedIndexBuffer
  , VectorSharedPtrIndexBuffer
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign

import Graphics.Urho3D.Graphics.Internal.IndexBuffer

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> sharedIndexBufferPtrCntx <> indexBufferCntx <> contextContext <> objectContext)
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
