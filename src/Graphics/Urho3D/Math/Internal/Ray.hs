module Graphics.Urho3D.Math.Internal.Ray(
    Ray(..)
  , rayCntx
  , HasOrigin(..)
  , HasDirection(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Vector3
import Control.Lens
import GHC.Generics
import Control.DeepSeq

-- | Infinite straight line in three-dimensional space.
data Ray = Ray {
  _rayOrigin    :: {-# UNPACK #-} !Vector3 -- ^ Ray origin.
, _rayDirection :: {-# UNPACK #-} !Vector3 -- ^ Ray direction.
} deriving (Eq, Ord, Show, Generic)

instance NFData Ray
makeFields ''Ray

rayCntx :: C.Context
rayCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Ray", [t| Ray |])
    ]
  }
