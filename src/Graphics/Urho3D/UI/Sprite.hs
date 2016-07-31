{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Sprite(
    Sprite 
  , spriteContext
  , SharedSprite
  -- | Setters
  , spriteSetPosition
  , spriteSetPosition'
  , spriteSetHotSpot
  , spriteSetHotSpot'
  , spriteSetScale
  , spriteSetScale'
  , spriteSetRotation
  , spriteSetTexture
  , spriteSetImageRect
  , spriteSetFullImageRect
  , spriteSetBlendMode
  -- | Getters
  , spriteGetPosition
  , spriteGetHotSpot
  , spriteGetScale
  , spriteGetRotation
  , spriteGetTexture
  , spriteGetImageRect
  , spriteGetBlendMode
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign 
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Graphics.Texture 
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Math.Vector2 
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.Sprite
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> sharedSpritePtrCntx <> spriteCntx <> contextContext <> uiElementContext <> textureContext <> vector2Context <> rectContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/UI/Sprite.h>"
C.using "namespace Urho3D"

spriteContext :: C.Context 
spriteContext = sharedSpritePtrCntx <> spriteCntx

newSprite :: Ptr Context -> IO (Ptr Sprite)
newSprite ptr = [C.exp| Sprite* { new Sprite( $(Context* ptr) ) } |]

deleteSprite :: Ptr Sprite -> IO ()
deleteSprite ptr = [C.exp| void { delete $(Sprite* ptr) } |]

instance Creatable (Ptr Sprite) where 
  type CreationOptions (Ptr Sprite) = Ptr Context 

  newObject = liftIO . newSprite
  deleteObject = liftIO . deleteSprite

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement] ''Sprite

instance UIElem Sprite where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = Sprite::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "Sprite"

-- | Set floating point position.
spriteSetPosition :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> Vector2 -- ^ value
  -> m ()
spriteSetPosition p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Sprite* ptr)->SetPosition(*$(Vector2* v'))} |]

-- | Set floating point position.
spriteSetPosition' :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> Float -- ^ X 
  -> Float -- ^ Y
  -> m ()
spriteSetPosition' p xv yv = liftIO $ do 
  let ptr = parentPointer p 
      xv' = realToFrac xv 
      yv' = realToFrac yv
  [C.exp| void {$(Sprite* ptr)->SetPosition($(float xv'), $(float yv'))} |]

-- | Set floating point position.
spriteSetHotSpot :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> IntVector2 -- ^ value
  -> m ()
spriteSetHotSpot p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Sprite* ptr)->SetHotSpot(*$(IntVector2* v'))} |]

-- | Set hotspot for positioning and rotation
spriteSetHotSpot' :: (Parent Sprite a, Pointer p a, MonadIO m) => p -- ^ Pointer to sprite
  -> Int -- ^ X 
  -> Int -- ^ Y 
  -> m ()
spriteSetHotSpot' ptr xv yv = liftIO $ do 
  let ptr' = parentPointer ptr 
      x' = fromIntegral xv 
      y' = fromIntegral yv
  [C.exp| void { $(Sprite* ptr')->SetHotSpot($(int x'), $(int y')) }|]

-- | Set scale. Scale also affects child sprites.
spriteSetScale :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> Vector2 -- ^ value
  -> m ()
spriteSetScale p v = liftIO $ with v $ \v' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Sprite* ptr)->SetScale(*$(Vector2* v'))} |]

-- | Set scale. Scale also affects child sprites.
spriteSetScale' :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> Float -- ^ X 
  -> Float -- ^ Y
  -> m ()
spriteSetScale' p xv yv = liftIO $ do 
  let ptr = parentPointer p 
      xv' = realToFrac xv 
      yv' = realToFrac yv
  [C.exp| void {$(Sprite* ptr)->SetScale($(float xv'), $(float yv'))} |]

-- | Set scale. Scale also affects child sprites.
spriteSetRotation :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> Float -- ^ value
  -> m ()
spriteSetRotation p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = realToFrac v
  [C.exp| void {$(Sprite* ptr)->SetRotation($(float v'))} |]

-- | Set Texture
spriteSetTexture :: (Parent Sprite a, Pointer p a, Parent Texture t, Pointer pt t, MonadIO m) 
  => p -- ^ Pointer to Sprite or child
  -> pt -- ^ Pointer to Texture or child
  -> m ()
spriteSetTexture ptr tex = liftIO $ do 
  let ptr' = parentPointer ptr 
      tex' = parentPointer tex
  [C.exp| void { $(Sprite* ptr')->SetTexture($(Texture* tex')) } |]

-- | Set part of texture to use as the image.
spriteSetImageRect :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> IntRect -- ^ value
  -> m ()
spriteSetImageRect p r = liftIO $ with r $ \r' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Sprite* ptr)->SetImageRect(*$(IntRect* r'))} |]

-- | Use whole texture as the image.
spriteSetFullImageRect :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> m ()
spriteSetFullImageRect p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void {$(Sprite* ptr)->SetFullImageRect()} |]

-- | Set scale. Scale also affects child sprites.
spriteSetBlendMode :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> BlendMode -- ^ value
  -> m ()
spriteSetBlendMode p bm = liftIO $ do 
  let ptr = parentPointer p 
      bm' = fromIntegral $ fromEnum bm
  [C.exp| void {$(Sprite* ptr)->SetBlendMode((BlendMode)$(int bm'))} |]

-- | Return floating point position.
spriteGetPosition :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> m Vector2
spriteGetPosition p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const Vector2* { &$(Sprite* ptr)->GetPosition() } |]

-- | Return hotspot.
spriteGetHotSpot :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> m IntVector2
spriteGetHotSpot p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const IntVector2* { &$(Sprite* ptr)->GetHotSpot() } |]

-- | Return scale.
spriteGetScale :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> m Vector2
spriteGetScale p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const Vector2* { &$(Sprite* ptr)->GetScale() } |]

-- | Return rotation angle.
spriteGetRotation :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> m Float
spriteGetRotation p = liftIO $ do 
  let ptr = parentPointer p 
  realToFrac <$> [C.exp| float { $(Sprite* ptr)->GetRotation() } |]

-- | Return texture.
spriteGetTexture :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> m (Maybe (Ptr Texture))
spriteGetTexture p = liftIO $ do 
  let ptr = parentPointer p 
  pt <- [C.exp| Texture* { $(Sprite* ptr)->GetTexture() } |]
  checkNullPtr' pt return

-- | Return image rectangle.
spriteGetImageRect :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> m IntRect
spriteGetImageRect p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const IntRect* { &$(Sprite* ptr)->GetImageRect() } |]

-- | Return rotation angle.
spriteGetBlendMode :: (Parent Sprite a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Sprite or child
  -> m BlendMode
spriteGetBlendMode p = liftIO $ do 
  let ptr = parentPointer p 
  toEnum . fromIntegral <$> [C.exp| int { (int)$(Sprite* ptr)->GetBlendMode() } |]