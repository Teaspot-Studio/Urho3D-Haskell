module Graphics.Urho3D.Graphics.Internal.Graphics(
    Graphics
  , graphicsCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Graphics 

graphicsCntx :: C.Context 
graphicsCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Graphics", [t| Graphics |])
    ]
  } 