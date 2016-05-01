module Graphics.Urho3D.Graphics.Internal.ModelMorph(
    ModelMorph(..)
  , VertexBufferMorph(..)
  , HasName(..)
  , HasWeight(..)
  , HasBuffers(..)
  , HasElementMask(..)
  , HasElementVertexCount(..)
  , HasElementDataSize(..)
  , HasMorphData(..)
  , modelMorphCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map
import Control.Lens 
import GHC.Generics
import qualified Data.Vector.Unboxed as V 
import qualified Data.HashMap.Strict as H 
import Control.DeepSeq
import Data.Word 

-- | Vertex buffer morph data.
data VertexBufferMorph = VertexBufferMorph {
  _vertexBufferMorphElementMask :: {-# UNPACK #-} !Word -- ^ Vertex elements 
, _vertexBufferMorphElementVertexCount :: {-# UNPACK #-} !Word -- ^ Number of vertices
, _vertexBufferMorphElementDataSize :: {-# UNPACK #-} !Word -- ^ Morphed vertices data size as bytes.
, _vertexBufferMorphMorphData :: {-# UNPACK #-} !(V.Vector Word8) -- ^ Morphed vertices. Stored packed as <index, data> pairs.
} deriving (Generic)

-- | Definition of a model's vertex morph.
data ModelMorph = ModelMorph {
  _modelMorphName :: !String -- ^ Morph name 
, _modelMorphWeight :: !Float -- ^ Current morph weight
, _modelMorphBuffers :: !(H.HashMap Word VertexBufferMorph) -- ^ Morph data per vertex buffer
} deriving (Generic)

makeFields ''VertexBufferMorph
makeFields ''ModelMorph

instance NFData VertexBufferMorph
instance NFData ModelMorph

modelMorphCntx :: C.Context 
modelMorphCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "ModelMorph", [t| ModelMorph |])
    , (C.TypeName "VertexBufferMorph", [t| VertexBufferMorph |])
    ]
  } 