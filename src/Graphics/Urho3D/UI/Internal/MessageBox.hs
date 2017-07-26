module Graphics.Urho3D.UI.Internal.MessageBox(
    MessageBox
  , messageBoxCntx
  , sharedMessageBoxPtrCntx
  , SharedMessageBox
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data MessageBox

messageBoxCntx :: C.Context 
messageBoxCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "MessageBox", [t| MessageBox |])
    ]
  }

sharedPtrImpl "MessageBox"
