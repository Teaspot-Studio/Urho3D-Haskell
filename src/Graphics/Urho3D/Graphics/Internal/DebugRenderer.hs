module Graphics.Urho3D.Graphics.Internal.DebugRenderer(
    DebugRenderer
  , debugRendererCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

-- | Debug geometry rendering component. Should be added only to the root scene node.
data DebugRenderer 


debugRendererCntx :: C.Context 
debugRendererCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "DebugRenderer", [t| DebugRenderer |])
    ]
  } 