{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Resource.Internal.XMLElement(
    XMLElement 
  , xmlElementCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data XMLElement

xmlElementCntx :: C.Context 
xmlElementCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "XMLElement", [t| XMLElement |])
    ]
  } 