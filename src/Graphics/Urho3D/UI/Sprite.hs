{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Sprite(
    Sprite 
  , spriteContext
  , SharedSprite
  , SharedSpritePtr 
  , spriteSetTexture
  , spriteSetScale
  , spriteSetHotSpot
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Sprite
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Graphics.Texture 
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> sharedSpritePtrCntx <> spriteCntx <> contextContext <> uiElementContext <> textureContext)
C.include "<Urho3D/UI/Sprite.h>"
C.using "namespace Urho3D"

spriteContext :: C.Context 
spriteContext = sharedSpritePtrCntx <> spriteCntx

newSprite :: Ptr Context -> IO (Ptr Sprite)
newSprite ptr = [C.exp| Sprite* { new Sprite( $(Context* ptr) ) } |]

deleteSprite :: Ptr Sprite -> IO ()
deleteSprite ptr = [C.exp| void { delete $(Sprite* ptr) } |]

instance Createable (Ptr Sprite) where 
  type CreationOptions (Ptr Sprite) = Ptr Context 

  newObject = liftIO . newSprite
  deleteObject = liftIO . deleteSprite

instance Parent UIElement Sprite  where 
  castToParent ptr = [C.pure| UIElement* {(UIElement*)$(Sprite* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Sprite* {(Sprite*)$(UIElement* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance UIElem Sprite where 
  uiElemType _ = unsafePerformIO $ [C.block| StringHash* { 
      static StringHash h = Sprite::GetTypeStatic();  
      return &h;
    } |]

sharedPtr "Sprite"

spriteSetTexture :: (Pointer p Sprite, MonadIO m) => p -> Ptr Texture -> m ()
spriteSetTexture ptr tex = liftIO $ do 
  let ptr' = pointer ptr 
  [C.exp| void { $(Sprite* ptr')->SetTexture($(Texture* tex)) } |]

spriteSetScale :: (Pointer p Sprite, MonadIO m) => p -> Float -> m ()
spriteSetScale ptr v = liftIO $ do 
  let ptr' = pointer ptr 
      v' = realToFrac v
  [C.exp| void { $(Sprite* ptr')->SetScale($(float v')) }|]

-- | Set hotspot for positioning and rotation
spriteSetHotSpot :: (Pointer p Sprite, MonadIO m) => p -- ^ Pointer to sprite
  -> Int -- ^ X 
  -> Int  -- ^ Y 
  -> m ()
spriteSetHotSpot ptr x y = liftIO $ do 
  let ptr' = pointer ptr 
      x' = fromIntegral x 
      y' = fromIntegral y
  [C.exp| void { $(Sprite* ptr')->SetHotSpot($(int x'), $(int y')) }|]