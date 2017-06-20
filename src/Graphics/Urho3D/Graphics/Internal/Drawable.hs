module Graphics.Urho3D.Graphics.Internal.Drawable(
    Drawable
  , PODVectorDrawablePtr
  , VectorSourceBatch
  , vectorSourceBatchCntx
  , SourceBatch(..)
  , HasDistance(..)
  , HasGeometry(..)
  , HasMaterial(..)
  , HasWorldTransform(..)
  , HasNumWorldTransforms(..)
  , HasGeometryType(..)
  , FrameInfo(..)
  , HasFrameNumber(..)
  , HasTimeStep(..)
  , HasViewSize(..)
  , HasCamera(..)
  , drawableCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import GHC.Generics
import Control.Lens
import Control.DeepSeq
import qualified Data.Map as Map
import Foreign

import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.Internal.Vector2
import Graphics.Urho3D.Math.Internal.Matrix3x4
import Graphics.Urho3D.Graphics.Internal.Camera
import Graphics.Urho3D.Graphics.Internal.Geometry
import Graphics.Urho3D.Graphics.Internal.Material
import Graphics.Urho3D.Graphics.Defs

-- | Base class for visible components.
data Drawable
data PODVectorDrawablePtr

-- | Source data for a 3D geometry draw call.
data SourceBatch = SourceBatch {
  _sourceBatchDistance :: {-# UNPACK #-} !Float -- ^ Distance from camera
, _sourceBatchGeometry :: {-# UNPACK #-} !(Ptr Geometry) -- ^ Geometry
, _sourceBatchMaterial :: {-# UNPACK #-} !(SharedPtr Material) -- ^ Material
, _sourceBatchWorldTransform :: {-# UNPACK #-} !(Ptr Matrix3x4) -- ^ World transform(s). For a skinned model, these are the bone transforms.
, _sourceBatchNumWorldTransforms :: {-# UNPACK #-} !Word -- ^ Number of world transforms.
, _sourceBatchGeometryType :: !GeometryType -- ^ Geometry type.
} deriving Generic

-- | Rendering frame update parameters.
data FrameInfo = FrameInfo {
  _frameInfoFrameNumber :: {-# UNPACK #-} !Word -- ^ frame number
, _frameInfoTimeStep :: {-# UNPACK #-} !Float -- ^ Time elapsed since last frame
, _frameInfoViewSize :: {-# UNPACK #-} !IntVector2 -- ^ Viewport size
, _frameInfoCamera :: {-# UNPACK #-} !(Ptr Camera) -- ^ Camera being used.
} deriving Generic

makeFields ''SourceBatch
makeFields ''FrameInfo

instance NFData SourceBatch where
  rnf SourceBatch{..} =
    _sourceBatchDistance `deepseq`
    _sourceBatchGeometry `seq`
    _sourceBatchMaterial `seq`
    _sourceBatchWorldTransform `seq`
    _sourceBatchNumWorldTransforms `deepseq`
    _sourceBatchGeometryType `deepseq` ()

instance NFData FrameInfo where
  rnf FrameInfo{..} =
    _frameInfoFrameNumber `deepseq`
    _frameInfoTimeStep `deepseq`
    _frameInfoViewSize `deepseq`
    _frameInfoCamera `seq` ()

drawableCntx :: C.Context
drawableCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Drawable", [t| Drawable |])
    , (C.TypeName "SourceBatch", [t| SourceBatch |])
    , (C.TypeName "FrameInfo", [t| FrameInfo |])
    , (C.TypeName "PODVectorDrawablePtr", [t| PODVectorDrawablePtr |])
    ]
  }

simpleVectorImpl "SourceBatch"
