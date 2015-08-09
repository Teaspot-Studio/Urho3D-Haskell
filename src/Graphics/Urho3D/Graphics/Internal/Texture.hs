{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Graphics.Internal.Texture(
    Texture
  , textureCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Texture 

textureCntx :: C.Context 
textureCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Texture", [t| Texture |])
    ]
  } 