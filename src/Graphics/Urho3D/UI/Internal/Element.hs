module Graphics.Urho3D.UI.Internal.Element(
    UIElement
  , HUIElement
  , uiElementCntx
  , sharedUIElementPtrCntx
  , SharedUIElement
  , weakUIElementPtrCntx
  , WeakUIElement
  , PODVectorUIElementPtr
  , podVectorUIElementPtrCntx
  , VectorUIElementPtr
  , vectorUIElementPtrCntx
  , sharedHUIElementPtrCntx
  , SharedHUIElement
  , weakHUIElementPtrCntx
  , WeakHUIElement
  , PODVectorHUIElementPtr
  , podVectorHUIElementPtrCntx
  , VectorHUIElementPtr
  , vectorHUIElementPtrCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import qualified Data.Map as Map

-- | Base class for UI elements.
data UIElement
-- | Ancestor of 'UIElement' that allows set callbacks on virtual functions
data HUIElement

uiElementCntx :: C.Context
uiElementCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "UIElement", [t| UIElement |])
    , (C.TypeName "HUIElement", [t| HUIElement |])
    ]
  }

sharedPtrImpl "UIElement"
sharedWeakPtrImpl "UIElement"
podVectorPtrImpl "UIElement"
vectorPtrImpl "UIElement"

sharedPtrImpl "HUIElement"
sharedWeakPtrImpl "HUIElement"
podVectorPtrImpl "HUIElement"
vectorPtrImpl "HUIElement"
