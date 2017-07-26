module Graphics.Urho3D.UI.Internal.ProgressBar(
    ProgressBar
  , progressBarCntx
  , sharedProgressBarPtrCntx
  , SharedProgressBar
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

-- | ProgressBar bar UI element.
data ProgressBar

progressBarCntx :: C.Context
progressBarCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "ProgressBar", [t| ProgressBar |])
    ]
  }

sharedPtrImpl "ProgressBar"
