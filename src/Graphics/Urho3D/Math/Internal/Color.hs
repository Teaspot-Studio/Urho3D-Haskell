module Graphics.Urho3D.Math.Internal.Color(
    Color(..)
  , colorCntx
  , HasRComp(..)
  , HasGComp(..)
  , HasBComp(..)
  , HasAComp(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Control.Lens 

data Color = Color {
  _colorRComp :: {-# UNPACK #-} !Float 
, _colorGComp :: {-# UNPACK #-} !Float 
, _colorBComp :: {-# UNPACK #-} !Float   
, _colorAComp :: {-# UNPACK #-} !Float   
} deriving (Eq, Show)
makeFields ''Color

colorCntx :: C.Context 
colorCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Color", [t| Color |])
    ]
  }