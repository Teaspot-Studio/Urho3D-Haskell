module Graphics.Urho3D.Graphics.Internal.Octree(
    Octree
  , Octant
  , octreeCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Octree 
data Octant

octreeCntx :: C.Context 
octreeCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Octree", [t| Octree |])
    , (C.TypeName "Octant", [t| Octant |])
    ]
  } 