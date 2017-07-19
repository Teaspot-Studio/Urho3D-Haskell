module Graphics.Urho3D.Math.Internal.Rect(
    Rect(..)
  , IntRect(..)
  , rectCntx
  , HasMinPoint(..)
  , HasMaxPoint(..)
  , HasLeft(..)
  , HasTop(..)
  , HasRight(..)
  , HasBottom(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Vector2
import Control.Lens
import GHC.Generics
import Control.DeepSeq

data Rect = Rect {
  _rectMinPoint :: {-# UNPACK #-} !Vector2
, _rectMaxPoint :: {-# UNPACK #-} !Vector2
} deriving (Eq, Ord, Generic, Show)
makeFields ''Rect
instance NFData Rect

data IntRect = IntRect {
  _intRectLeft :: {-# UNPACK #-} !Int
, _intRectTop :: {-# UNPACK #-} !Int
, _intRectRight :: {-# UNPACK #-} !Int
, _intRectBottom :: {-# UNPACK #-} !Int
} deriving (Eq, Ord, Generic, Show)
makeFields ''IntRect
instance NFData IntRect

rectCntx :: C.Context
rectCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Rect", [t| Rect |])
    , (C.TypeName "IntRect", [t| IntRect |])
    ]
  }
