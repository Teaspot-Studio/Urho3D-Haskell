module Graphics.Urho3D.Math.Internal.Vector3(
    Vector3(..)
  , vector3Cntx
  , HasX(..)
  , HasY(..)
  , HasZ(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Vector2
import Control.Lens 

data Vector3 = Vector3 {
  _vector3X :: Float 
, _vector3Y :: Float 
, _vector3Z :: Float   
} deriving (Eq, Ord, Show)
makeFields ''Vector3

vector3Cntx :: C.Context 
vector3Cntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Vector3", [t| Vector3 |])
    ]
  }