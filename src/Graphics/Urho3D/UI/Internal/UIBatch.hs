module Graphics.Urho3D.UI.Internal.UIBatch(
    UIBatch(..)
  , HasElement(..)
  , HasBlendMode(..)
  , HasScissor(..)
  , HasTexture(..)
  , HasInvTextureSize(..)
  , HasColor(..)
  , HasVertexData (..)
  , HasVertexStart(..)
  , HasVertexEnd(..)
  , HasUseGradient(..)
  , uiBatchCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as V

import Control.Lens
import Foreign
import GHC.Generics
import Graphics.Urho3D.Graphics.Internal.Defs
import Graphics.Urho3D.Graphics.Internal.Texture
import Graphics.Urho3D.Math.Internal.Rect
import Graphics.Urho3D.Math.Internal.Vector2
import Graphics.Urho3D.UI.Internal.Element

-- resolve field conflicts
import Graphics.Urho3D.UI.Internal.Cursor
import Graphics.Urho3D.Graphics.Internal.BillboardSet

-- | UI rendering draw call.
data UIBatch = UIBatch {
  _uIBatchElement        :: !(Ptr UIElement) -- ^ Element this batch represents.
, _uIBatchBlendMode      :: !BlendMode -- ^ Blending mode
, _uIBatchScissor        :: !IntRect -- ^ Scissor rectangle.
, _uIBatchTexture        :: !(Ptr Texture) -- ^ Texture.
, _uIBatchInvTextureSize :: !Vector2 -- ^ Inverse texture size.
, _uIBatchColor          :: !Word -- ^ Current color. By default calculated from the element.
, _uIBatchVertexData     :: !(V.Vector Float) -- ^ Vertex data.
, _uIBatchVertexStart    :: !Word -- ^ Vertex data start index
, _uIBatchVertexEnd      :: !Word -- ^ Vertex data end index
, _uIBatchUseGradient    :: !Bool -- ^ Gradient flag
} deriving (Show, Generic)
makeFields ''UIBatch

uiBatchCntx :: C.Context
uiBatchCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "UIBatch", [t| UIBatch |])
    ]
  }
