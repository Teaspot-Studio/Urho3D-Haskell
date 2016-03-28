module Graphics.Urho3D.Resource.Internal.Cache(
    ResourceCache 
  , resourceCacheCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data ResourceCache

resourceCacheCntx :: C.Context 
resourceCacheCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "ResourceCache", [t| ResourceCache |])
    ]
  } 