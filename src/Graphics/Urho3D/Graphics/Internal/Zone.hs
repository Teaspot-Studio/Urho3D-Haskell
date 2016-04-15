module Graphics.Urho3D.Graphics.Internal.Zone(
    Zone
  , zoneCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Zone 

zoneCntx :: C.Context 
zoneCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Zone", [t| Zone |])
    ]
  } 