module Graphics.Urho3D.UI.Internal.ScrollView(
    ScrollView
  , scrollViewCntx
  , sharedScrollViewPtrCntx
  , SharedScrollView
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data ScrollView

scrollViewCntx :: C.Context
scrollViewCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "ScrollView", [t| ScrollView |])
    ]
  }

sharedPtrImpl "ScrollView"
