module Graphics.Urho3D.IO.Internal.Serializer(
    Serializer
  , serializerCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Serializer 

serializerCntx :: C.Context 
serializerCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Serializer", [t| Serializer |])
    ]
  } 