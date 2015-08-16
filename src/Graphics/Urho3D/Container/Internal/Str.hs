{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Container.Internal.Str(
    UrhoString
  , UrhoWString
  , stringCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data UrhoString
data UrhoWString

stringCntx :: C.Context 
stringCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "String", [t| UrhoString |])
    , (C.TypeName "WString", [t| UrhoWString |])
    ]
  } 