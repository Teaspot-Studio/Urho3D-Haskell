{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Resource.Image(
    Image
  , imageContext
  , imageSavePNG
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Resource.Internal.Image
import Graphics.Urho3D.Resource.Resource
import Data.Monoid
import Foreign
import Foreign.C.String

C.context (C.cppCtx <> imageCntx <> contextContext <> resourceContext)
C.include "<Urho3D/Resource/Image.h>"
C.using "namespace Urho3D"

imageContext :: C.Context
imageContext = imageCntx <> resourceContext

newImage :: Ptr Context -> IO (Ptr Image)
newImage ptr = [C.exp| Image* { new Image( $(Context* ptr) ) } |]

deleteImage :: Ptr Image -> IO ()
deleteImage ptr = [C.exp| void { delete $(Image* ptr) } |]

instance Creatable (Ptr Image) where
  type CreationOptions (Ptr Image) = Ptr Context

  newObject = liftIO . newImage
  deleteObject = liftIO . deleteImage

instance ResourceType Image where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Image::GetTypeStatic().Value() } |]

-- | Saves image as PNG
imageSavePNG :: (Pointer p a, Parent Image a, MonadIO m) => p -- ^ Pointer to image or child
  -> String -- ^ path to file with extension
  -> m ()
imageSavePNG p path = liftIO $ withCString path $ \path' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Image* ptr)->SavePNG(String($(const char* path')))} |]
