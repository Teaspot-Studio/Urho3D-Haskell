module Graphics.Urho3D.UI.Internal.Button(
    Button 
  , buttonCntx
  , sharedButtonPtrCntx
  , SharedButton
  , SharedButtonPtr(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Button

buttonCntx :: C.Context 
buttonCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Button", [t| Button |])
    ]
  } 

sharedPtrImpl "Button"