module Graphics.Urho3D.Network.Internal.Network(
    Network
  , networkCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data Network

networkCntx :: C.Context 
networkCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Network", [t| Network |])
    ]
  }
