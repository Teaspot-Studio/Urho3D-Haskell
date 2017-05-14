module Graphics.Urho3D.Graphics.Internal.Geometry(
    Geometry
  , VectorSharedPtrGeometry
  , VectorVectorSharedPtrGeometry
  , geometryCntx
  , SharedGeometry
  , sharedGeometryPtrCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr

import qualified Data.Map as Map

-- | Defines one or more vertex buffers, an index buffer and a draw range.
data Geometry
data VectorSharedPtrGeometry
data VectorVectorSharedPtrGeometry

geometryCntx :: C.Context
geometryCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Geometry", [t| Geometry |])
    , (C.TypeName "VectorSharedPtrGeometry", [t| VectorSharedPtrGeometry |])
    , (C.TypeName "VectorVectorSharedPtrGeometry", [t| VectorVectorSharedPtrGeometry |])
    ]
  }

sharedPtrImpl "Geometry"
