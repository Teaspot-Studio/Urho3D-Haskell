{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.VertexBuffer(
    VertexBuffer 
  , vertexBufferContext
  , SharedVertexBuffer
  , SharedVertexBufferPtr 
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign

import Graphics.Urho3D.Graphics.Internal.VertexBuffer

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent 

C.context (C.cppCtx <> sharedVertexBufferPtrCntx <> vertexBufferCntx <> contextContext <> objectContext)
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