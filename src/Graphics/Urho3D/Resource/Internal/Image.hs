module Graphics.Urho3D.Resource.Internal.Image(
    Image
  , PODVectorImagePtr
  , SharedImage
  , VectorSharedImagePtr
  , WeakImage
  , imageCntx
  , podVectorImagePtrCntx
  , sharedImagePtrCntx
  , weakImagePtrCntx
  ) where

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

data Image
data VectorSharedImagePtr

imageCntx :: C.Context
imageCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Image", [t| Image |])
    , (C.TypeName "VectorSharedImagePtr", [t| VectorSharedImagePtr |])
    ]
  }

sharedPtrImpl "Image"
sharedWeakPtrImpl "Image"
podVectorPtrImpl "Image"
