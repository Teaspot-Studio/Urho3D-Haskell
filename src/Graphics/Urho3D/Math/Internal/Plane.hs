module Graphics.Urho3D.Math.Internal.Plane(
    Plane(..)
  , planeCntx
  , HasNormal(..)
  , HasAbsNormal(..)
  , HasConstant(..)
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
data Plane = Plane {
  _planeNormal :: {-# UNPACK #-} !Vector3 
, _planeAbsNormal :: {-# UNPACK #-} !Vector3 
, _planeConstant :: {-# UNPACK #-} !Float   
} deriving (Eq, Ord, Show, Generic)

instance NFData Plane
makeFields ''Plane

planeCntx :: C.Context 
planeCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Plane", [t| Plane |])
    ]
  }