{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Engine.Internal.DebugHud(
    DebugHud
  , debugHudCntx
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data DebugHud

debugHudCntx :: C.Context 
debugHudCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "DebugHud", [t| DebugHud |])
    ]
  }
