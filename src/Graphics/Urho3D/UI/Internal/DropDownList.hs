module Graphics.Urho3D.UI.Internal.DropDownList(
    DropDownList
  , dropDownListCntx
  , sharedDropDownListPtrCntx
  , SharedDropDownList
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data DropDownList

dropDownListCntx :: C.Context 
dropDownListCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "DropDownList", [t| DropDownList |])
    ]
  }

sharedPtrImpl "DropDownList"
