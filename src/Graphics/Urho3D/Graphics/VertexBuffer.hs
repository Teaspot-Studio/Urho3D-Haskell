{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.VertexBuffer(
    VertexBuffer
  , vertexBufferContext
  , SharedVertexBuffer
  , VectorSharedPtrVertexBuffer
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

import Graphics.Urho3D.Graphics.Internal.VertexBuffer

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> sharedVertexBufferPtrCntx <> vertexBufferCntx <> contextContext <> objectContext)
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
