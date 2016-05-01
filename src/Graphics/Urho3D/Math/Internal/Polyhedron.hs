module Graphics.Urho3D.Math.Internal.Polyhedron(
    Polyhedron(..)
  , polyhedronCntx
  , HasFaces(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Vector3
import Control.Lens 
import GHC.Generics
import Control.DeepSeq

import qualified Data.Vector as V 
import qualified Data.Vector.Unboxed as VU 

-- | Surface in three-dimensional space.
data Polyhedron = Polyhedron {
  _polyhedronFaces :: {-# UNPACK #-} !(V.Vector (VU.Vector Vector3))
} deriving (Eq, Ord, Show, Generic)

instance NFData Polyhedron
makeFields ''Polyhedron

polyhedronCntx :: C.Context 
polyhedronCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Polyhedron", [t| Polyhedron |])
    ]
  }