module Graphics.Urho3D.Math.Internal.Sphere(
    Sphere(..)
  , sphereCntx
  , HasCenter(..)
  , HasRadius(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Vector3
import Control.Lens 
import GHC.Generics
import Control.DeepSeq

-- | Surface in three-dimensional space.
data Sphere = Sphere {
  _sphereCenter :: {-# UNPACK #-} !Vector3 
, _sphereRadius :: {-# UNPACK #-} !Float 
} deriving (Eq, Ord, Show, Generic)

instance NFData Sphere
makeFields ''Sphere

sphereCntx :: C.Context 
sphereCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Sphere", [t| Sphere |])
    ]
  }