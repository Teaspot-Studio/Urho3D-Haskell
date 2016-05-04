module Graphics.Urho3D.Engine.Internal.Engine(
    Engine
  , engineCntx
  , SharedEngine
  , sharedEnginePtrCntx
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr 

import qualified Data.Map as Map

data Engine

engineCntx :: C.Context 
engineCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Engine", [t| Engine |])
    ]
  }

sharedPtrImpl "Engine"