module Graphics.Urho3D.Math.Internal.Frustum(
    Frustum(..)
  , frustumCntx
  , HasNear(..)
  , HasLeft(..)
  , HasRight(..)
  , HasUp(..)
  , HasDown(..)
  , HasFar(..)
  , HasVert1(..)
  , HasVert2(..)
  , HasVert3(..)
  , HasVert4(..)
  , HasVert5(..)
  , HasVert6(..)
  , HasVert7(..)
  , HasVert8(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Plane
import Graphics.Urho3D.Math.Internal.Vector3
import Graphics.Urho3D.Math.Internal.Rect
import Control.Lens 
import GHC.Generics
import Control.DeepSeq

-- | Convex constructed of 6 planes.
data Frustum = Frustum {
  _frustumNear :: {-# UNPACK #-} !Plane 
, _frustumLeft :: {-# UNPACK #-} !Plane 
, _frustumRight :: {-# UNPACK #-} !Plane   
, _frustumUp :: {-# UNPACK #-} !Plane
, _frustumDown :: {-# UNPACK #-} !Plane  
, _frustumFar :: {-# UNPACK #-} !Plane
, _frustumVert1 :: {-# UNPACK #-} !Vector3
, _frustumVert2 :: {-# UNPACK #-} !Vector3
, _frustumVert3 :: {-# UNPACK #-} !Vector3
, _frustumVert4 :: {-# UNPACK #-} !Vector3
, _frustumVert5 :: {-# UNPACK #-} !Vector3
, _frustumVert6 :: {-# UNPACK #-} !Vector3
, _frustumVert7 :: {-# UNPACK #-} !Vector3
, _frustumVert8 :: {-# UNPACK #-} !Vector3
} deriving (Eq, Ord, Show, Generic)

instance NFData Frustum
makeFields ''Frustum

frustumCntx :: C.Context 
frustumCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Frustum", [t| Frustum |])
    ]
  }