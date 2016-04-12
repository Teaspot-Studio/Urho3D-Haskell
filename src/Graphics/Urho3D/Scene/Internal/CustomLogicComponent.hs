module Graphics.Urho3D.Scene.Internal.CustomLogicComponent(
    CustomLogicComponent
  , customLogicComponentCntx
  , sharedCustomLogicComponentPtrCntx
  , SharedCustomLogicComponent
  , SharedCustomLogicComponentPtr(..)
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data CustomLogicComponent

customLogicComponentCntx :: C.Context 
customLogicComponentCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "CustomLogicComponent", [t| CustomLogicComponent |])
    ]
  }

sharedPtrImpl "CustomLogicComponent"