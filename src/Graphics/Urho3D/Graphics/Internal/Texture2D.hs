module Graphics.Urho3D.Graphics.Internal.Texture2D(
    Texture2D
  , SharedTexture2D
  , WeakTexture2D
  , VectorSharedTexture2DPtr
  , PODVectorTexture2DPtr
  , texture2DCntx
  , podVectorTexture2DPtrCntx
  , sharedTexture2DPtrCntx
  , weakTexture2DPtrCntx
  ) where

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

data Texture2D
data VectorSharedTexture2DPtr

texture2DCntx :: C.Context
texture2DCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Texture2D", [t| Texture2D |])
    , (C.TypeName "VectorSharedTexture2DPtr", [t| VectorSharedTexture2DPtr |])
    ]
  }

sharedPtrImpl "Texture2D"
sharedWeakPtrImpl "Texture2D"
podVectorPtrImpl "Texture2D"
