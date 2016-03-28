module Graphics.Urho3D.Graphics.Internal.Camera(
    Camera
  , cameraCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Camera 

cameraCntx :: C.Context 
cameraCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Camera", [t| Camera |])
    ]
  } 