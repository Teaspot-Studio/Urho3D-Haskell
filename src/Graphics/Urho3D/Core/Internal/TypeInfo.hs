module Graphics.Urho3D.Core.Internal.TypeInfo(
    TypeInfo
  , typeInfoCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data TypeInfo

typeInfoCntx :: C.Context 
typeInfoCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "TypeInfo", [t| TypeInfo |])
    ]
  } 