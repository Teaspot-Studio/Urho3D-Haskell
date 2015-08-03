{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Math.Internal.StringHash(
    StringHash 
  , stringHashCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data StringHash

stringHashCntx :: C.Context 
stringHashCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "StringHash", [t| StringHash |])
    ]
  }