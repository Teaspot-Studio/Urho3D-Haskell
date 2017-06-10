module Graphics.Urho3D.Graphics.Internal.Texture2DArray(
    Texture2DArray
  , SharedTexture2DArray
  , WeakTexture2DArray
  , VectorSharedTexture2DArrayPtr
  , PODVectorTexture2DArrayPtr
  , texture2DArrayCntx
  , podVectorTexture2DArrayPtrCntx
  , sharedTexture2DArrayPtrCntx
  , weakTexture2DArrayPtrCntx
  ) where

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Texture2DArray
data VectorSharedTexture2DArrayPtr

texture2DArrayCntx :: C.Context
texture2DArrayCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Texture2DArray", [t| Texture2DArray |])
    , (C.TypeName "VectorSharedTexture2DArrayPtr", [t| VectorSharedTexture2DArrayPtr |])
    ]
  }

sharedPtrImpl "Texture2DArray"
sharedWeakPtrImpl "Texture2DArray"
podVectorPtrImpl "Texture2DArray"
