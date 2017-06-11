module Graphics.Urho3D.Graphics.Internal.Technique(
    Technique
  , techniqueCntx
  , SharedTechnique
  , sharedTechniquePtrCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Technique

techniqueCntx :: C.Context
techniqueCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Technique", [t| Technique |])
    ]
  }

sharedPtrImpl "Technique"
