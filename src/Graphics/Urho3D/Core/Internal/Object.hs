{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Core.Internal.Object(
    Object
  , objectCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Object

objectCntx :: C.Context 
objectCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Object", [t| Object |])
    ]
  } 