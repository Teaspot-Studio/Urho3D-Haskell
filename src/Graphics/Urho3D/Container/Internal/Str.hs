module Graphics.Urho3D.Container.Internal.Str(
    UrhoString
  , UrhoWString
  , stringCntx
  , StringVector
  , WStringVector
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data UrhoString
data UrhoWString

data StringVector 
data WStringVector

stringCntx :: C.Context 
stringCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "String", [t| UrhoString |])
    , (C.TypeName "WString", [t| UrhoWString |])
    , (C.TypeName "StringVector", [t| StringVector |])
    , (C.TypeName "WStringVector", [t| WStringVector |])
    ]
  } 

