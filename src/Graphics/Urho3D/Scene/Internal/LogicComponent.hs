module Graphics.Urho3D.Scene.Internal.LogicComponent(
    LogicComponent
  , logicComponentCntx
  , sharedLogicComponentPtrCntx
  , SharedLogicComponent
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data LogicComponent

logicComponentCntx :: C.Context 
logicComponentCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "LogicComponent", [t| LogicComponent |])
    ]
  }

sharedPtrImpl "LogicComponent" 