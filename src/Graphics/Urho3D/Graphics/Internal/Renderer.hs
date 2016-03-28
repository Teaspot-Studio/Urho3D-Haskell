module Graphics.Urho3D.Graphics.Internal.Renderer(
    Renderer
  , rendererCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Renderer 

rendererCntx :: C.Context 
rendererCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Renderer", [t| Renderer |])
    ]
  } 