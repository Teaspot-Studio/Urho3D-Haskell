module Graphics.Urho3D.UI.Internal.Text(
    Text
  , textCntx
  , sharedTextPtrCntx
  , SharedText
  , CharLocation(..)
  , GlyphLocation(..)
  , HasPosition(..)
  , HasSize(..)
  , HasX(..)
  , HasY(..)
  , HasGlyph(..)
  ) where

import Control.Lens
import Foreign
import GHC.Generics
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.UI.Internal.FontFace
import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

data Text

-- | Cached character location and size within text. Used for queries related to text editing.
data CharLocation = CharLocation {
  _charLocationPosition :: {-# UNPACK #-} !IntVector2 -- ^ Position.
, _charLocationSize     :: {-# UNPACK #-} !IntVector2 -- ^ Size
} deriving (Eq, Show, Generic)

makeFields ''CharLocation

-- | Glyph and its location within the text. Used when preparing text rendering.
data GlyphLocation = GlyphLocation {
  _glyphLocationX :: {-# UNPACK #-} !Int -- ^ X coordinate.
, _glyphLocationY :: {-# UNPACK #-} !Int -- ^ Y coordinate.
, _glyphLocationGlyph :: {-# UNPACK #-} !(Ptr FontGlyph)
} deriving (Eq, Show, Generic)

makeFields ''GlyphLocation

textCntx :: C.Context
textCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Text", [t| Text |])
    , (C.TypeName "CharLocation", [t| CharLocation |])
    , (C.TypeName "GlyphLocation", [t| GlyphLocation |])
    ]
  }

sharedPtrImpl "Text"
