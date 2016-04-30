module Graphics.Urho3D.Scene.Internal.Component(
    Component
  , componentCntx
  , sharedComponentPtrCntx
  , sharedWeakComponentPtrCntx
  , SharedComponent
  , SharedComponentPtr(..)
  , SharedWeakComponent
  , SharedWeakComponentPtr(..)
  , VectorSharedComponentPtr
  , VectorSharedWeakComponentPtr
  , PODVectorComponentPtr
  , podVectorComponentPtrCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import qualified Data.Map as Map

data Component
data VectorSharedComponentPtr
data VectorSharedWeakComponentPtr

componentCntx :: C.Context 
componentCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Component", [t| Component |])
    , (C.TypeName "VectorSharedComponentPtr", [t| VectorSharedComponentPtr |])
    , (C.TypeName "VectorSharedWeakComponentPtr", [t| VectorSharedWeakComponentPtr |])
    ]
  }

sharedPtrImpl "Component" 
sharedWeakPtrImpl "Component" 
podVectorPtrImpl "Component"