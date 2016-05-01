module Graphics.Urho3D.Math.Internal.Matrix4(
    Matrix4(..)
  , matrix4Cntx
  , HasRow1(..)
  , HasRow2(..)
  , HasRow3(..)
  , HasRow4(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

import Graphics.Urho3D.Math.Internal.Vector4
import Graphics.Urho3D.Math.Internal.Matrix3x4
import Control.Lens 
import Control.DeepSeq 
import GHC.Generics (Generic)

data Matrix4 = Matrix4 {
  _matrix4Row1 :: {-# UNPACK #-} !Vector4 
, _matrix4Row2 :: {-# UNPACK #-} !Vector4 
, _matrix4Row3 :: {-# UNPACK #-} !Vector4 
, _matrix4Row4 :: {-# UNPACK #-} !Vector4 
} deriving (Show, Eq, Generic)

makeFields ''Matrix4

instance NFData Matrix4

matrix4Cntx :: C.Context 
matrix4Cntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Matrix4", [t| Matrix4 |])
    ]
  }