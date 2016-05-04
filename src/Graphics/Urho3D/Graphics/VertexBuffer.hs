{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.VertexBuffer(
    VertexBuffer 
  , vertexBufferContext
  , SharedVertexBuffer
  , VectorSharedVertexBufferPtr
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign

import Graphics.Urho3D.Graphics.Internal.VertexBuffer

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent 

C.context (C.cppCtx <> sharedVertexBufferPtrCntx <> vertexBufferCntx <> contextContext <> objectContext)
C.include "<Urho3D/Core/Object.h>"
C.include "<Urho3D/Graphics/VertexBuffer.h>"
C.using "namespace Urho3D"

vertexBufferContext :: C.Context 
vertexBufferContext = sharedVertexBufferPtrCntx <> vertexBufferCntx <> objectContext

instance Createable (Ptr VertexBuffer) where 
  type CreationOptions (Ptr VertexBuffer) = Ptr Context

  newObject cntxPtr = liftIO $ [C.exp| VertexBuffer* { new VertexBuffer( $(Context* cntxPtr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(VertexBuffer* ptr) } |]

deriveParent ''Object ''VertexBuffer

sharedPtr "VertexBuffer"

C.verbatim "typedef Vector<SharedPtr<VertexBuffer> > VectorSharedVertexBufferPtr;"

instance Createable (Ptr VectorSharedVertexBufferPtr) where 
  type CreationOptions (Ptr VectorSharedVertexBufferPtr) = ()
  newObject _ = liftIO [C.exp| VectorSharedVertexBufferPtr* {new Vector<SharedPtr<VertexBuffer> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorSharedVertexBufferPtr* ptr) } |]

instance ReadableVector VectorSharedVertexBufferPtr where 
  type ReadVecElem VectorSharedVertexBufferPtr = SharedPtr VertexBuffer
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorSharedVertexBufferPtr* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peekSharedPtr =<< [C.exp| SharedVertexBuffer* { new SharedPtr<VertexBuffer>((*$(VectorSharedVertexBufferPtr* ptr))[$(unsigned int i')]) } |]
    where i' = fromIntegral i 

instance WriteableVector VectorSharedVertexBufferPtr where 
  type WriteVecElem VectorSharedVertexBufferPtr = SharedPtr VertexBuffer
  foreignVectorAppend ptr e = liftIO $ [C.exp| void {$(VectorSharedVertexBufferPtr* ptr)->Push(SharedPtr<VertexBuffer>($(VertexBuffer* e'))) } |]
    where e' = parentPointer e 