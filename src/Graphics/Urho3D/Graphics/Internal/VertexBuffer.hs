module Graphics.Urho3D.Graphics.Internal.VertexBuffer(
    VertexBuffer
  , vertexBufferCntx
  , sharedVertexBufferPtrCntx
  , SharedVertexBuffer
  , VectorSharedVertexBufferPtr
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr 
import qualified Data.Map as Map

data VertexBuffer 
data VectorSharedVertexBufferPtr

vertexBufferCntx :: C.Context 
vertexBufferCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "VertexBuffer", [t| VertexBuffer |])
    , (C.TypeName "VectorSharedVertexBufferPtr", [t| VectorSharedVertexBufferPtr |])
    ]
  }

sharedPtrImpl "VertexBuffer"