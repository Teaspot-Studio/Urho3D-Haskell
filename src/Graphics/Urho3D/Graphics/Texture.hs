{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Texture(
    Texture
  , textureContext
  , textureWidth
  , textureHeight
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Texture
import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> textureCntx <> stringHashContext)
C.include "<Urho3D/Graphics/Texture.h>"
C.using "namespace Urho3D"

textureContext :: C.Context 
textureContext = textureCntx

instance ResourceType Texture where 
  resourceType _ = unsafePerformIO $ [C.block| StringHash* { 
    static StringHash h = Texture::GetTypeStatic(); 
    return &h; 
    } |]

textureWidth, textureHeight :: (Pointer p Texture, MonadIO m) => p -> m Int 
textureWidth ptr = liftIO $ do 
  let ptr' = pointer ptr
  fromIntegral <$> [C.exp| int { $(Texture* ptr')->GetWidth() } |]
textureHeight ptr = liftIO $ do 
  let ptr' = pointer ptr
  fromIntegral <$> [C.exp| int { $(Texture* ptr')->GetHeight() } |]