module Graphics.Urho3D.Graphics.Internal.BillboardSet(
    BillboardSet
  , Billboard(..)
  , HasPosition(..)
  , HasSize(..)
  , HasUv(..)
  , HasColor(..)
  , HasRotation(..)
  , HasDirection(..)
  , HasEnabled(..)
  , HasSortDistance(..)
  , HasScreenScaleFactor(..)
  , billboardSetCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Math.Internal.Color
import Graphics.Urho3D.Math.Internal.Rect
import Graphics.Urho3D.Math.Internal.Vector2
import Graphics.Urho3D.Math.Internal.Vector3

-- For lenses conflicts
import Graphics.Urho3D.Graphics.Animation

import Control.DeepSeq
import Control.Lens
import GHC.Generics
import qualified Data.Map as Map

-- | Billboard component.
data BillboardSet

-- | One billboard in the billboard set.
data Billboard = Billboard {
  _billboardPosition          :: {-# UNPACK #-} !Vector3 -- ^ Position
, _billboardSize              :: {-# UNPACK #-} !Vector2 -- ^ Two-dimensional size. If BillboardSet has fixed screen size enabled, this is measured in pixels instead of world units.
, _billboardUv                :: {-# UNPACK #-} !Rect    -- ^ UV coordinates.
, _billboardColor             :: {-# UNPACK #-} !Color   -- ^ Color.
, _billboardRotation          :: {-# UNPACK #-} !Float   -- ^ Rotation.
, _billboardDirection         :: {-# UNPACK #-} !Vector3 -- ^ Direction (For direction based billboard only).
, _billboardEnabled           ::                !Bool    -- ^ Enabled flag.
, _billboardSortDistance      :: {-# UNPACK #-} !Float   -- ^ Sort distance. Used internally.
, _billboardScreenScaleFactor :: {-# UNPACK #-} !Float   -- ^ Scale factor for fixed screen size mode. Used internally.
} deriving (Eq, Show, Generic)

makeFields ''Billboard
instance NFData Billboard

billboardSetCntx :: C.Context
billboardSetCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "BillboardSet", [t| BillboardSet |])
    , (C.TypeName "Billboard", [t| Billboard |])
    ]
  }
