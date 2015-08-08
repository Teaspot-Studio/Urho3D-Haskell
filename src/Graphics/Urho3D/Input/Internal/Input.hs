{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Input.Internal.Input(
    Input
  , inputCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Input 

inputCntx :: C.Context 
inputCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Input", [t| Input |])
    ]
  } 