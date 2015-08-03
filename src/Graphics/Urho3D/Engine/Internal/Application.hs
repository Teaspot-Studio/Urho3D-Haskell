{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Engine.Internal.Application(
    Application
  , applicationCntx
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Application

applicationCntx :: C.Context 
applicationCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Application", [t| Application |])
    ]
  }