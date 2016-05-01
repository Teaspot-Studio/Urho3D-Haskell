module Graphics.Urho3D.Graphics.Internal.Skeleton(
    Skeleton
  , skeletonCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Skeleton 

skeletonCntx :: C.Context 
skeletonCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Skeleton", [t| Skeleton |])
    ]
  } 