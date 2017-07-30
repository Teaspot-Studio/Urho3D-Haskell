module Graphics.Urho3D.UI.Internal.View3D(
    View3D
  , view3DCntx
  , sharedView3DPtrCntx
  , weakView3DPtrCntx
  , SharedView3D
  , WeakView3D
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data View3D

view3DCntx :: C.Context
view3DCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "View3D", [t| View3D |])
    ]
  }

sharedPtrImpl "View3D"
sharedWeakPtrImpl "View3D"
