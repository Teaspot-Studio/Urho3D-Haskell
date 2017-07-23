module Graphics.Urho3D.UI.Internal.Slider(
    Slider
  , sliderCntx
  , sharedSliderPtrCntx
  , SharedSlider
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Slider

sliderCntx :: C.Context
sliderCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Slider", [t| Slider |])
    ]
  }

sharedPtrImpl "Slider"
