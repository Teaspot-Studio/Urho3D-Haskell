module Graphics.Urho3D.Graphics.Internal.Light(
    Light
  , PODVectorLightPtr
  , lightCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Light 
data PODVectorLightPtr

lightCntx :: C.Context 
lightCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Light", [t| Light |])
    , (C.TypeName "PODVectorLightPtr", [t| PODVectorLightPtr |])
    ]
  } 