module Graphics.Urho3D.Engine.Internal.Application(
    ApplicationH
  , Application
  , applicationCntx
  , sharedApplicationHPtrCntx
  , SharedApplicationH
  , SharedApplicationHPtr(..)
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr 
import qualified Data.Map as Map

data ApplicationH
type Application = ApplicationH

applicationCntx :: C.Context 
applicationCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "ApplicationH", [t| ApplicationH |])
    ]
  }

sharedPtrImpl "ApplicationH"