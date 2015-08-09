{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Resource.Internal.XMLFile(
    XMLFile 
  , xmlFileCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data XMLFile

xmlFileCntx :: C.Context 
xmlFileCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "XMLFile", [t| XMLFile |])
    ]
  } 