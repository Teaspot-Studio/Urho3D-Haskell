{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Resource.Internal.Image(
    Image 
  , imageCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data Image

imageCntx :: C.Context 
imageCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Image", [t| Image |])
    ]
  } 