module Graphics.Urho3D.UI.Internal.Text3D(
    Text3D
  , text3DCntx
  , sharedText3DPtrCntx
  , SharedText3D
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Text3D

text3DCntx :: C.Context
text3DCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Text3D", [t| Text3D |])
    ]
  }

sharedPtrImpl "Text3D"
