module Graphics.Urho3D.Resource.Internal.Resource(
    Resource 
  , resourceCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data Resource

resourceCntx :: C.Context 
resourceCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Resource", [t| Resource |])
    ]
  } 