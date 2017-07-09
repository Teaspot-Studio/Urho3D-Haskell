module Graphics.Urho3D.Scene.Internal.Node(
    Node
  , nodeCntx
  , sharedNodePtrCntx
  , weakNodePtrCntx
  , SharedNode
  , WeakNode
  , VectorSharedNodePtr
  , PODVectorNodePtr
  , podVectorNodePtrCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import qualified Data.Map as Map

data Node
data VectorSharedNodePtr

nodeCntx :: C.Context
nodeCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Node", [t| Node |])
    , (C.TypeName "VectorSharedNodePtr", [t| VectorSharedNodePtr |])
    ]
  }

sharedPtrImpl "Node"
sharedWeakPtrImpl "Node"
podVectorPtrImpl "Node"
