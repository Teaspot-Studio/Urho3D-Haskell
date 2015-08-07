{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.IO.Internal.FileSystem(
    FileSystem
  , fileSystemCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data FileSystem 

fileSystemCntx :: C.Context 
fileSystemCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "FileSystem", [t| FileSystem |])
    ]
  } 