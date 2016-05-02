module Graphics.Urho3D.Graphics.Internal.Geometry(
    Geometry
  , geometryCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

-- | Defines one or more vertex buffers, an index buffer and a draw range.
data Geometry 

geometryCntx :: C.Context 
geometryCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Geometry", [t| Geometry |])
    ]
  } 