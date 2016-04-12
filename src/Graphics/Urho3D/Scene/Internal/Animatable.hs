module Graphics.Urho3D.Scene.Internal.Animatable(
    Animatable
  , animatableCntx
  , sharedAnimatablePtrCntx
  , SharedAnimatable
  , SharedAnimatablePtr(..)
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Animatable

animatableCntx :: C.Context 
animatableCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Animatable", [t| Animatable |])
    ]
  }

sharedPtrImpl "Animatable" 