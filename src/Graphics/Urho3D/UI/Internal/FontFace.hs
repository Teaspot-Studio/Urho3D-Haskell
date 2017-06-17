module Graphics.Urho3D.UI.Internal.FontFace(
    FontFace
  , fontFaceCntx
  , FontGlyph(..)
  , HasX(..)
  , HasY(..)
  , HasWidth(..)
  , HasHeight(..)
  , HasOffsetX(..)
  , HasOffsetY(..)
  , HasAdvanceX(..)
  , HasPage(..)
  , HasUsed(..)
  ) where

import Control.Lens
import Data.Word
import GHC.Generics
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.UI.Events
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data FontFace

fontFaceCntx :: C.Context
fontFaceCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "FontFace", [t| FontFace |])
    , (C.TypeName "FontGlyph", [t| FontGlyph |])
    ]
  }

-- | Font glyph description.
data FontGlyph = FontGlyph {
  _fontGlyphX :: {-# UNPACK #-} !Word16 -- ^ X position in texture
, _fontGlyphY :: {-# UNPACK #-} !Word16 -- ^ Y position in texture
, _fontGlyphWidth :: {-# UNPACK #-} !Word16 -- ^ Width
, _fontGlyphHeight :: {-# UNPACK #-} !Word16 -- ^ Height
, _fontGlyphOffsetX :: {-# UNPACK #-} !Word16 -- ^ Glyph X offset from origin
, _fontGlyphOffsetY :: {-# UNPACK #-} !Word16 -- ^ Glyph Y offset from origin
, _fontGlyphAdvanceX :: {-# UNPACK #-} !Word16 -- ^ Horizontal advance
, _fontGlyphPage :: {-# UNPACK #-} !Word -- ^ Texture page. M_MAX_UNSIGNED if not yet resident on any texture.
, _fontGlyphUsed :: !Bool -- ^ Used flag
} deriving (Eq, Generic)

makeFields ''FontGlyph
