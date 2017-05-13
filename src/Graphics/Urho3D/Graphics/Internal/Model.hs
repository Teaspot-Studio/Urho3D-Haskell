module Graphics.Urho3D.Graphics.Internal.Model(
    Model
  , VertexBufferDesc(..)
  , IndexBufferDesc(..)
  , GeometryDesc(..)
  , HasVertexCount(..)
  , HasVertexElements(..)
  , HasIndexCount(..)
  , HasIndexSize(..)
  , HasDatum(..)
  , HasPType(..)
  , HasVbRef(..)
  , HasIbRef(..)
  , HasIndexStart(..)
  , modelCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Control.Lens
import Data.Vector (Vector)
import Data.Word
import GHC.Generics
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Graphics.Defs

import qualified Data.Map.Strict as Map

data Model

-- | Description of vertex buffer data for asynchronous loading.
data VertexBufferDesc = VertexBufferDesc {
  _vertexBufferDescVertexCount    :: !Word -- ^ Vertex count
, _vertexBufferDescVertexElements :: !(Vector VertexElement) -- ^ Vertex declaration
, _vertexBufferDescDatum          :: !(SharedArrayPtr Word8) -- ^ Vertex data
} deriving (Generic)

makeFields ''VertexBufferDesc

-- | Description of index buffer data for asynchronous loading.
data IndexBufferDesc = IndexBufferDesc {
  _indexBufferDescIndexCount :: !Word -- ^ Index count
, _indexBufferDescIndexSize  :: !Word -- ^ Index size
, _indexBufferDescDatum      :: !(SharedArrayPtr Word8) -- ^ Index data
} deriving (Generic)
makeFields ''IndexBufferDesc

-- | Description of a geometry for asynchronous loading.
data GeometryDesc = GeometryDesc {
  _geometryDescPType       :: !PrimitiveType -- ^ Primitive type
, _geometryDescVbRef       :: !Word -- ^ Vertex buffer ref
, _geometryDescIbRef       :: !Word -- ^ Index buffer ref
, _geometryDescIndexStart  :: !Word -- ^ Index start
, _geometryDescIndexCount  :: !Word -- ^ Index count
} deriving (Generic)
makeFields ''GeometryDesc

modelCntx :: C.Context
modelCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Model", [t| Model |])
    , (C.TypeName "VertexBufferDesc", [t| VertexBufferDesc |])
    , (C.TypeName "IndexBufferDesc", [t| IndexBufferDesc |])
    , (C.TypeName "GeometryDesc", [t| GeometryDesc |])
    ]
  }
