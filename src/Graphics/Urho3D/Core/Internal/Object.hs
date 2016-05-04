module Graphics.Urho3D.Core.Internal.Object(
    Object
  , HaskellHandler
  , objectCntx
  , sharedObjectPtrCntx
  , SharedObject
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr

import qualified Data.Map as Map

data Object
data HaskellHandler

objectCntx :: C.Context 
objectCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Object", [t| Object |])
    , (C.TypeName "HaskellHandler", [t| HaskellHandler |])
    ]
  } 

sharedPtrImpl "Object"