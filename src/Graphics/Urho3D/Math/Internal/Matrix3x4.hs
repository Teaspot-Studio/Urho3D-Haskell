module Graphics.Urho3D.Math.Internal.Matrix3x4(
    Matrix3x4(..)
  , matrix3x4Cntx
  , HasRow1(..)
  , HasRow2(..)
  , HasRow3(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

import Graphics.Urho3D.Math.Internal.Vector4
import Control.Lens 
import Control.DeepSeq 
import GHC.Generics (Generic)

data Matrix3x4 = Matrix3x4 {
  _matrix3x4Row1 :: {-# UNPACK #-} !Vector4 
, _matrix3x4Row2 :: {-# UNPACK #-} !Vector4 
, _matrix3x4Row3 :: {-# UNPACK #-} !Vector4 
} deriving (Show, Eq, Generic)

makeFields ''Matrix3x4

instance NFData Matrix3x4

matrix3x4Cntx :: C.Context 
matrix3x4Cntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Matrix3x4", [t| Matrix3x4 |])
    ]
  }