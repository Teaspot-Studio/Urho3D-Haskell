{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Network.Internal.Connection(
    Connection 
  , connectionCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data Connection

connectionCntx :: C.Context 
connectionCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Connection", [t| Connection |])
    ]
  } 