module Graphics.Urho3D.Math.Internal.BoundingBox(
    BoundingBox(..)
  , boundingBoxCntx
  , HasMinVector(..)
  , HasMaxVector(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Vector3
import Control.Lens 

data BoundingBox = BoundingBox {
  _boundingBoxMinVector :: {-# UNPACK #-} !Vector3 
, _boundingBoxMaxVector :: {-# UNPACK #-} !Vector3
} deriving (Eq, Ord, Show)
makeFields ''BoundingBox

boundingBoxCntx :: C.Context 
boundingBoxCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "BoundingBox", [t| BoundingBox |])
    ]
  }