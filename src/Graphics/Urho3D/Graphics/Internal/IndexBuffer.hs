module Graphics.Urho3D.Graphics.Internal.IndexBuffer(
    IndexBuffer
  , indexBufferCntx
  , sharedIndexBufferPtrCntx
  , SharedIndexBuffer
  , VectorSharedPtrIndexBuffer
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data IndexBuffer
data VectorSharedPtrIndexBuffer

indexBufferCntx :: C.Context
indexBufferCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "IndexBuffer", [t| IndexBuffer |])
    , (C.TypeName "VectorSharedPtrIndexBuffer", [t| VectorSharedPtrIndexBuffer |])
    ]
  }

sharedPtrImpl "IndexBuffer"
