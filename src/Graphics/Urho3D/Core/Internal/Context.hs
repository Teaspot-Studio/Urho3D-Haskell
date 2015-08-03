{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Core.Internal.Context(
    Context
  , contextCntx
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data Context

contextCntx :: C.Context 
contextCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Context", [t| Context |])
    ]
  }