module Graphics.Urho3D.Graphics.Internal.VertexBuffer(
    VertexBuffer
  , vertexBufferCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data VertexBuffer 

vertexBufferCntx :: C.Context 
vertexBufferCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "VertexBuffer", [t| VertexBuffer |])
    ]
  } 