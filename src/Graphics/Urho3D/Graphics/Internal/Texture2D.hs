{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Graphics.Internal.Texture2D(
    Texture2D
  , texture2DCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Texture2D 

texture2DCntx :: C.Context 
texture2DCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Texture2D", [t| Texture2D |])
    ]
  } 