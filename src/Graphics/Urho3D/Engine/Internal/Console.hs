{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Engine.Internal.Console(
    Console
  , consoleCntx
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Console

consoleCntx :: C.Context 
consoleCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Console", [t| Console |])
    ]
  }
