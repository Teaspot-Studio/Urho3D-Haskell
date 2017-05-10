module Graphics.Urho3D.Graphics.Internal.Model(
    Model
  , modelCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import GHC.Generics

import qualified Data.Map as Map

data Model

-- | Description of vertex buffer data for asynchronous loading.
data VertexBufferDesc = VertexBufferDesc {

} deriving (Generic)

-- | Description of index buffer data for asynchronous loading.
data IndexBufferDesc = IndexBufferDesc {

} deriving (Generic)

-- | Description of a geometry for asynchronous loading.
data GeometryDesc = GeometryDesc {

} deriving (Generic)

modelCntx :: C.Context
modelCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Model", [t| Model |])
    ]
  }
