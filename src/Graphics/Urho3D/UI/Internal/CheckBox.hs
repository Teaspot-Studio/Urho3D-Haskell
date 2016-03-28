module Graphics.Urho3D.UI.Internal.CheckBox(
    CheckBox 
  , checkBoxCntx
  , sharedCheckBoxPtrCntx
  , SharedCheckBox
  , SharedCheckBoxPtr(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data CheckBox

checkBoxCntx :: C.Context 
checkBoxCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "CheckBox", [t| CheckBox |])
    ]
  } 

sharedPtrImpl "CheckBox"