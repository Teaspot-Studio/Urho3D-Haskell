{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.UI.Internal.BorderImage(
    BorderImage 
  , borderImageCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

data BorderImage

borderImageCntx :: C.Context 
borderImageCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "BorderImage", [t| BorderImage |])
    ]
  } 