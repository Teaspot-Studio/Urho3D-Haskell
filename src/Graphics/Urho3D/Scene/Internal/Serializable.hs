module Graphics.Urho3D.Scene.Internal.Serializable(
    Serializable
  , serializableCntx
  , sharedSerializablePtrCntx
  , SharedSerializable
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Serializable

serializableCntx :: C.Context 
serializableCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Serializable", [t| Serializable |])
    ]
  }

sharedPtrImpl "Serializable" 