module Graphics.Urho3D.Graphics.Internal.Drawable(
    Drawable
  , drawableCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Drawable 

drawableCntx :: C.Context 
drawableCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Drawable", [t| Drawable |])
    ]
  } 