{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.BorderImage(
    BorderImage 
  , borderImageContext
  , borderImageSetTexture
  , borderImageSetImageRect
  , borderImageSetFullImageRect
  , borderImageSetBorder
  , borderImageSetImageBorder
  , borderImageSetHoverOffset
  , borderImageSetBlendMode
  , borderImageSetTiled
  , borderImageGetTexture
  , borderImageGetImageRect
  , borderImageGetBorder
  , borderImageGetHoverOffset
  , borderImageGetBlendMode
  , borderImageIsTiled
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Math.Rect 
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.Internal.BorderImage
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> borderImageCntx <> contextContext <> uiElementContext <> rectContext <> vector2Context <> textureContext)
C.include "<Urho3D/UI/BorderImage.h>"
C.using "namespace Urho3D"

borderImageContext :: C.Context 
borderImageContext = borderImageCntx

newBorderImage :: Ptr Context -> IO (Ptr BorderImage)
newBorderImage ptr = [C.exp| BorderImage* { new BorderImage( $(Context* ptr) ) } |]

deleteBorderImage :: Ptr BorderImage -> IO ()
deleteBorderImage ptr = [C.exp| void { delete $(BorderImage* ptr) } |]

instance Createable (Ptr BorderImage) where 
  type CreationOptions (Ptr BorderImage) = Ptr Context 

  newObject = liftIO . newBorderImage
  deleteObject = liftIO . deleteBorderImage

instance Parent UIElement BorderImage  where 
  castToParent ptr = [C.pure| UIElement* {(UIElement*)$(BorderImage* ptr)} |]
  castToChild ptr = let
    child = [C.pure| BorderImage* {(BorderImage*)$(UIElement* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance UIElem BorderImage where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = BorderImage::GetTypeStatic();  
      return &h;
    } |]

-- | Sets element texture
borderImageSetTexture :: (Parent BorderImage a, Pointer p a, Parent Texture b, Pointer pTex b, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> pTex -- ^ Pointer to Texture or child
  -> m ()
borderImageSetTexture p t = liftIO $ do 
  let ptr = parentPointer p 
      tex = parentPointer t 
  [C.exp| void { $(BorderImage* ptr)->SetTexture($(Texture* tex)) } |]

-- | Set part of texture to use as the image.
borderImageSetImageRect :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> IntRect -- ^ Part of texture to use as the image
  -> m ()
borderImageSetImageRect p r = liftIO $ with r $ \r' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(BorderImage* ptr)->SetImageRect(*$(IntRect* r'))} |]

-- | Use whole texture as the image.
borderImageSetFullImageRect :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> m ()
borderImageSetFullImageRect p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void {$(BorderImage* ptr)->SetFullImageRect()} |]

-- | Set border dimensions on the screen.
borderImageSetBorder :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> IntRect -- ^ Border margins
  -> m ()
borderImageSetBorder p r = liftIO $ with r $ \r' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(BorderImage* ptr)->SetBorder(*$(IntRect* r'))} |]

-- | Set border dimensions on the image. If zero (default) uses the screen dimensions, resulting in pixel-perfect borders.
borderImageSetImageBorder :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> IntRect -- ^ Border margins
  -> m ()
borderImageSetImageBorder p r = liftIO $ with r $ \r' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(BorderImage* ptr)->SetImageBorder(*$(IntRect* r'))} |]

-- | Set offset to image rectangle used on hover.
borderImageSetHoverOffset :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> IntVector2 -- ^ Offset
  -> m ()
borderImageSetHoverOffset p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(BorderImage* ptr)->SetHoverOffset(*$(IntVector2* v'))} |]

-- | Set blend mode.
borderImageSetBlendMode :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> BlendMode -- ^ blend mode
  -> m ()
borderImageSetBlendMode p bm = liftIO $ do 
  let ptr = parentPointer p 
      bm' = fromIntegral $ fromEnum bm
  [C.exp| void {$(BorderImage* ptr)->SetBlendMode((BlendMode)$(int bm'))} |]

-- | Set tiled mode.
borderImageSetTiled :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> Bool -- ^ enable
  -> m ()
borderImageSetTiled p b = liftIO $ do 
  let ptr = parentPointer p 
      b' = fromBool b
  [C.exp| void {$(BorderImage* ptr)->SetTiled($(int b') != 0)} |]

-- | Return texture.
borderImageGetTexture :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> m (Ptr Texture)
borderImageGetTexture p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| Texture* { $(BorderImage* ptr)->GetTexture() } |]

-- | Return image rectangle.
borderImageGetImageRect :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> m IntRect
borderImageGetImageRect p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const IntRect* { &$(BorderImage* ptr)->GetImageRect() } |]

-- | Return border screen dimensions.
borderImageGetBorder :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> m IntRect
borderImageGetBorder p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const IntRect* { &$(BorderImage* ptr)->GetBorder() } |]

-- | Return border screen dimensions.
borderImageGetHoverOffset :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> m IntVector2
borderImageGetHoverOffset p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const IntVector2* { &$(BorderImage* ptr)->GetHoverOffset() } |]

-- | Return blend mode.
borderImageGetBlendMode :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> m BlendMode
borderImageGetBlendMode p = liftIO $ do 
  let ptr = parentPointer p 
  toEnum . fromIntegral <$> [C.exp| int { (int)$(BorderImage* ptr)->GetBlendMode() } |]

-- | Return whether is tiled.
borderImageIsTiled :: (Parent BorderImage a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to BorderImage or child
  -> m Bool
borderImageIsTiled p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { (int)$(BorderImage* ptr)->IsTiled() } |]