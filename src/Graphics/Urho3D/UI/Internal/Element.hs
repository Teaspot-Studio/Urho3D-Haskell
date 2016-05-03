module Graphics.Urho3D.UI.Internal.Element(
    UIElement 
  , uiElementCntx
  , sharedUIElementPtrCntx
  , SharedUIElement
  , SharedUIElementPtr(..)
  , sharedWeakUIElementPtrCntx
  , SharedWeakUIElement
  , SharedWeakUIElementPtr(..)
  , PODVectorUIElementPtr
  , podVectorUIElementPtrCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import qualified Data.Map as Map

data UIElement

uiElementCntx :: C.Context 
uiElementCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "UIElement", [t| UIElement |])
    ]
  } 

sharedPtrImpl "UIElement"
sharedWeakPtrImpl "UIElement"
podVectorPtrImpl "UIElement"