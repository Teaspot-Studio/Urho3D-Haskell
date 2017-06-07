module Graphics.Urho3D.Graphics.Internal.Texture2DArray(
    Texture2DArray
  , texture2DArrayCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Texture2DArray

texture2DArrayCntx :: C.Context 
texture2DArrayCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Texture2DArray", [t| Texture2DArray |])
    ]
  }
