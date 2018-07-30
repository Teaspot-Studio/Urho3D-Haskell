module Graphics.Urho3D.Network.Internal.Connection(
    Connection
  , SharedConnection
  , VectorSharedPtrConnection
  , connectionCntx
  , sharedConnectionPtrCntx
  ) where

import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

data Connection
data VectorSharedPtrConnection

connectionCntx :: C.Context
connectionCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Connection", [t| Connection |])
    , (C.TypeName "VectorSharedPtrConnection", [t| VectorSharedPtrConnection |])
    ]
  }

sharedPtrImpl "Connection"
