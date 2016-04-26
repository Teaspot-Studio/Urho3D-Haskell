module Graphics.Urho3D.Math.Internal.Vector4(
    Vector4(..)
  , vector4Cntx
  , HasX(..)
  , HasY(..)
  , HasZ(..)
  , HasW(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Vector2
import Graphics.Urho3D.Math.Internal.Vector3
import Graphics.Urho3D.Math.Internal.Quaternion
import Control.Lens 
import GHC.Generics 
import Control.DeepSeq

data Vector4 = Vector4 {
  _vector4X :: {-# UNPACK #-} !Float 
, _vector4Y :: {-# UNPACK #-} !Float 
, _vector4Z :: {-# UNPACK #-} !Float   
, _vector4W :: {-# UNPACK #-} !Float  
} deriving (Eq, Ord, Show, Generic)

makeFields ''Vector4

instance NFData Vector4 

vector4Cntx :: C.Context 
vector4Cntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Vector4", [t| Vector4 |])
    ]
  }