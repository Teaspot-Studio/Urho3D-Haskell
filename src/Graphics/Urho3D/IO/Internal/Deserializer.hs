module Graphics.Urho3D.IO.Internal.Deserializer(
    Deserializer
  , deserializerCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Deserializer 

deserializerCntx :: C.Context 
deserializerCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Deserializer", [t| Deserializer |])
    ]
  } 