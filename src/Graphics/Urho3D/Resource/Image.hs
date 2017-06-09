{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Resource.Image(
    Image
  , SharedImage
  , WeakImage
  , VectorSharedImagePtr
  , PODVectorImagePtr
  , imageContext
  , imageSavePNG
  , imageSetSize2D
  , imageSetData
  , imageSetPixel2D
  , imageSetPixel2DInt
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Foreign.C.String
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Resource.Internal.Image
import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx
  <> imageCntx
  <> contextContext
  <> resourceContext
  <> colorContext
  <> sharedImagePtrCntx
  <> weakImagePtrCntx
  <> podVectorImagePtrCntx
  )
C.include "<Urho3D/Resource/Image.h>"
C.using "namespace Urho3D"

C.verbatim "typedef Vector<SharedPtr<Image> > VectorSharedImagePtr;"

imageContext :: C.Context
imageContext = imageCntx
  <> resourceContext
  <> sharedImagePtrCntx

sharedPtr "Image"
sharedWeakPtr "Image"
podVectorPtr "Image"

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

instance Creatable (Ptr VectorSharedImagePtr) where
  type CreationOptions (Ptr VectorSharedImagePtr) = ()

  newObject _ = liftIO [C.exp| VectorSharedImagePtr* { new VectorSharedImagePtr() } |]
  deleteObject ptr = liftIO $ [C.exp| void {delete $(VectorSharedImagePtr* ptr)} |]

instance ReadableVector VectorSharedImagePtr where
  type ReadVecElem VectorSharedImagePtr = SharedPtr Image
  foreignVectorLength ptr = fromIntegral <$>
    liftIO [C.exp| unsigned int {$(VectorSharedImagePtr* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    peekSharedPtr =<< [C.exp| SharedImage* { new SharedPtr<Image>((*$(VectorSharedImagePtr* ptr))[$(int i')]) } |]

instance WriteableVector VectorSharedImagePtr where
  type WriteVecElem VectorSharedImagePtr = SharedPtr Image
  foreignVectorAppend ptr sp = liftIO $ do
    let p = pointer sp
    [C.exp| void { $(VectorSharedImagePtr* ptr)->Push(SharedPtr<Image>($(Image* p))) } |]

-- | Saves image as PNG
imageSavePNG :: (Pointer p a, Parent Image a, MonadIO m) => p -- ^ Pointer to image or child
  -> String -- ^ path to file with extension
  -> m ()
imageSavePNG p path = liftIO $ withCString path $ \path' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Image* ptr)->SavePNG(String($(const char* path')))} |]

-- | Set 2D size and number of color components. Old image data will be destroyed and new data is undefined. Return true if successful.
imageSetSize2D :: (Pointer p a, Parent Image a, MonadIO m)
  => p -- ^ Pointer to image or child
  -> Int -- ^ width
  -> Int -- ^ height
  -> Word -- ^ components
  -> m Bool
imageSetSize2D p width height components = liftIO $ do
  let ptr = parentPointer p
      width' = fromIntegral width
      height' = fromIntegral height
      components' = fromIntegral components
  toBool <$> [C.exp| int {(int)$(Image* ptr)->SetSize($(int width'), $(int height'), $(unsigned int components'))} |]

-- | Set new image data.
imageSetData :: (Pointer p a, Parent Image a, MonadIO m)
  => p -- ^ Pointer to image or child
  -> Ptr () -- ^ Pixel data
  -> m ()
imageSetData p datum = liftIO $  do
  let ptr = parentPointer p
      datum' = castPtr datum
  [C.exp| void {$(Image* ptr)->SetData($(const unsigned char* datum'))} |]

-- | Set a 2D pixel.
imageSetPixel2D :: (Pointer p a, Parent Image a, MonadIO m)
  => p -- ^ Pointer to image or child
  -> Int -- ^ x
  -> Int -- ^ y
  -> Color -- ^ color
  -> m ()
imageSetPixel2D p x y color = liftIO $ with color $ \color' -> do
  let ptr = parentPointer p
      x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void {$(Image* ptr)->SetPixel($(int x'), $(int y'), *$(Color* color'))} |]

-- | Set a 2D pixel with an integer color. R component is in the 8 lowest bits.
imageSetPixel2DInt :: (Pointer p a, Parent Image a, MonadIO m)
  => p -- ^ Pointer to image or child
  -> Int -- ^ x
  -> Int -- ^ y
  -> Word -- ^ uint color
  -> m ()
imageSetPixel2DInt p x y uintColor = liftIO $ do
  let ptr = parentPointer p
      x' = fromIntegral x
      y' = fromIntegral y
      uintColor' = fromIntegral uintColor
  [C.exp| void {$(Image* ptr)->SetPixelInt($(int x'), $(int y'), $(unsigned int uintColor'))} |]
