module Graphics.Urho3D.Math.Internal.Quaternion(
    Quaternion(..)
  , quaternionCntx
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
import Control.Lens 
import Control.DeepSeq 
import GHC.Generics (Generic)

data Quaternion = Quaternion {
  _quaternionX :: Float 
, _quaternionY :: Float 
, _quaternionZ :: Float 
, _quaternionW :: Float
} deriving (Show, Eq, Generic)

makeFields ''Quaternion

instance NFData Quaternion

quaternionCntx :: C.Context 
quaternionCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Quaternion", [t| Quaternion |])
    ]
  }