{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.Texture2DArray(
    Texture2DArray
  , SharedTexture2DArray
  , WeakTexture2DArray
  , VectorSharedTexture2DArrayPtr
  , PODVectorTexture2DArrayPtr
  , texture2DArrayContext
  , texture2DArraySetLayers
  , texture2DArraySetSize
  , texture2DArraySetData
  , texture2DArraySetDataFromImage
  , texture2DArrayGetLayers
  , texture2DArrayGetData
  , texture2DArrayGetRenderSurface
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Graphics.Internal.Texture2DArray
import Graphics.Urho3D.Math.StringHash

import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Graphics.RenderSurface
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.Image
import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx
  <> contextContext
  <> texture2DArrayCntx
  <> textureContext
  <> stringHashContext
  <> objectContext
  <> resourceContext
  <> imageContext
  <> renderSurfaceContext
  <> sharedTexture2DArrayPtrCntx
  <> weakTexture2DArrayPtrCntx
  <> podVectorTexture2DArrayPtrCntx
  )
C.include "<Urho3D/Graphics/Texture2DArray.h>"
C.using "namespace Urho3D"

C.verbatim "typedef Vector<SharedPtr<Texture2DArray> > VectorSharedTexture2DArrayPtr;"

texture2DArrayContext :: C.Context
texture2DArrayContext = texture2DArrayCntx <> textureContext

sharedPtr "Texture2DArray"
sharedWeakPtr "Texture2DArray"
podVectorPtr "Texture2DArray"

newTexture2DArray :: Ptr Context -> IO (Ptr Texture2DArray)
newTexture2DArray ptr = [C.exp| Texture2DArray* { new Texture2DArray( $(Context* ptr) ) } |]

deleteTexture2DArray :: Ptr Texture2DArray -> IO ()
deleteTexture2DArray ptr = [C.exp| void { delete $(Texture2DArray* ptr) } |]

instance Creatable (Ptr Texture2DArray) where
  type CreationOptions (Ptr Texture2DArray) = Ptr Context

  newObject = liftIO . newTexture2DArray
  deleteObject = liftIO . deleteTexture2DArray

instance ResourceType Texture2DArray where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Texture2DArray::GetTypeStatic().Value() } |]

deriveParents [''Object, ''Resource, ''Texture] ''Texture2DArray

instance Creatable (Ptr VectorSharedTexture2DArrayPtr) where
  type CreationOptions (Ptr VectorSharedTexture2DArrayPtr) = ()

  newObject _ = liftIO [C.exp| VectorSharedTexture2DArrayPtr* { new VectorSharedTexture2DArrayPtr() } |]
  deleteObject ptr = liftIO $ [C.exp| void {delete $(VectorSharedTexture2DArrayPtr* ptr)} |]

instance ReadableVector VectorSharedTexture2DArrayPtr where
  type ReadVecElem VectorSharedTexture2DArrayPtr = SharedPtr Texture2DArray
  foreignVectorLength ptr = fromIntegral <$>
    liftIO [C.exp| unsigned int {$(VectorSharedTexture2DArrayPtr* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    peekSharedPtr =<< [C.exp| SharedTexture2DArray* { new SharedPtr<Texture2DArray>((*$(VectorSharedTexture2DArrayPtr* ptr))[$(int i')]) } |]

instance WriteableVector VectorSharedTexture2DArrayPtr where
  type WriteVecElem VectorSharedTexture2DArrayPtr = SharedPtr Texture2DArray
  foreignVectorAppend ptr sp = liftIO $ do
    let p = pointer sp
    [C.exp| void { $(VectorSharedTexture2DArrayPtr* ptr)->Push(SharedPtr<Texture2DArray>($(Texture2DArray* p))) } |]

-- | Set the number of layers in the texture. To be used before SetData.
texture2DArraySetLayers :: (Parent Texture2DArray a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to a Texture2DArray or ancestor
  -> Word -- ^ Number of layers
  -> m ()
texture2DArraySetLayers ptr n = liftIO $ do
  let ptr' = parentPointer ptr
      n' = fromIntegral n
  [C.exp| void { $(Texture2DArray* ptr')->SetLayers($(unsigned int n')) } |]
-- void SetLayers(unsigned layers);

-- | Set layers, size, format and usage. Set layers to zero to leave them unchanged. Return true if successful.
texture2DArraySetSize :: (Parent Texture2DArray a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to a Texture2DArray or ancestor
  -> Word -- ^ Number of layers
  -> Int -- ^ Width
  -> Int -- ^ Height
  -> Word -- ^ Format (?)
  -> TextureUsage -- ^ usage (default TextureStatic)
  -> m Bool
texture2DArraySetSize ptr n w h format usage = liftIO $ do
  let ptr' = parentPointer ptr
      n' = fromIntegral n
      w' = fromIntegral w
      h' = fromIntegral h
      format' = fromIntegral format
      usage' = fromIntegral . fromEnum $ usage
  toBool <$> [C.exp| int { (int)$(Texture2DArray* ptr')->SetSize($(unsigned int n'), $(int w'), $(int h'), $(unsigned int format'), (TextureUsage)$(int usage')) } |]
-- bool SetSize(unsigned layers, int width, int height, unsigned format, TextureUsage usage = TEXTURE_STATIC);

-- | Set data either partially or fully on a layer's mip level. Return true if successful.
texture2DArraySetData :: (Parent Texture2DArray a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to a Texture2DArray or ancestor
  -> Word -- ^ Layer
  -> Word -- ^ Level
  -> Int -- ^ x
  -> Int -- ^ y
  -> Int -- ^ width
  -> Int -- ^ height
  -> Ptr () -- ^ data
  -> m Bool
texture2DArraySetData ptr layer level x y width height datum = liftIO $ do
  let ptr'    = parentPointer ptr
      layer'  = fromIntegral layer
      level'  = fromIntegral level
      x'      = fromIntegral x
      y'      = fromIntegral y
      width'  = fromIntegral width
      height' = fromIntegral height
  toBool <$> [C.exp| int { (int)$(Texture2DArray* ptr')->SetData($(unsigned int layer'), $(unsigned int level'), $(int x'), $(int y'), $(int width'), $(int height'), $(void* datum)) } |]
-- bool SetData(unsigned layer, unsigned level, int x, int y, int width, int height, const void* data);

-- Set data of one layer from a stream. Return true if successful.
-- bool SetData(unsigned layer, Deserializer& source);

-- | Set data of one layer from an image. Return true if successful. Optionally make a single channel image alpha-only.
texture2DArraySetDataFromImage :: (Parent Texture2DArray a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to a Texture2DArray or ancestor
  -> Word -- ^ layer
  -> Ptr Image -- ^ image
  -> Bool -- ^ use alpha (default false)
  -> m Bool
texture2DArraySetDataFromImage ptr layer image useAlpha = liftIO $ do
  let ptr' = parentPointer ptr
      layer' = fromIntegral layer
      useAlpha' = fromBool useAlpha
  toBool <$> [C.exp| int { (int)$(Texture2DArray* ptr')->SetData($(unsigned int layer'), $(Image* image), $(int useAlpha') != 0) } |]
-- bool SetData(unsigned layer, Image* image, bool useAlpha = false);


-- | Return number of layers in the texture.
texture2DArrayGetLayers :: (Parent Texture2DArray a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to a Texture2DArray or ancestor
  -> m Word
texture2DArrayGetLayers ptr = liftIO $ do
  let ptr' = parentPointer ptr
  fromIntegral <$> [C.exp| unsigned int { $(Texture2DArray* ptr')->GetLayers() } |]
-- unsigned GetLayers() const { return layers_; }

-- | Get data from a mip level. The destination buffer must be big enough. Return true if successful.
texture2DArrayGetData :: (Parent Texture2DArray a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to a Texture2DArray or ancestor
  -> Word -- ^ layer
  -> Word -- ^ level
  -> Ptr () -- ^ destination
  -> m Bool
texture2DArrayGetData ptr layer level dest = liftIO $ do
  let ptr' = parentPointer ptr
      layer' = fromIntegral layer
      level' = fromIntegral level
  toBool <$> [C.exp| int { (int)$(Texture2DArray* ptr')->GetData($(unsigned int layer'), $(unsigned int level'), $(void* dest)) } |]
-- bool GetData(unsigned layer, unsigned level, void* dest) const;

-- | Return render surface.
texture2DArrayGetRenderSurface :: (Parent Texture2DArray a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to a Texture2DArray or ancestor
  -> m (Ptr RenderSurface)
texture2DArrayGetRenderSurface ptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| RenderSurface* { $(Texture2DArray* ptr')->GetRenderSurface() } |]
-- RenderSurface* GetRenderSurface() const { return renderSurface_; }
