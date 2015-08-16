{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Texture(
    Texture
  , textureContext
  , textureLevels
  , textureWidth
  , textureHeight
  , textureDepth
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Texture
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid

C.context (C.cppCtx <> textureCntx <> stringHashContext)
C.include "<Urho3D/Graphics/Texture.h>"
C.include "<iostream>"
C.using "namespace Urho3D"

textureContext :: C.Context 
textureContext = textureCntx

textureLevels :: (Parent Texture a, Pointer p a, MonadIO m) => p -> m Int 
textureLevels ptr = liftIO $ do 
  let ptr' = parentPointer ptr
  fromIntegral <$> [C.exp| int { $(Texture* ptr')->GetLevels() } |]

textureWidth :: (Parent Texture a, Pointer p a, MonadIO m) => p -> m Int 
textureWidth ptr = liftIO $ do 
  let ptr' = parentPointer ptr
  fromIntegral <$> [C.exp| int { $(Texture* ptr')->GetWidth() } |]

textureHeight :: (Parent Texture a, Pointer p a, MonadIO m) => p -> m Int 
textureHeight ptr = liftIO $ do 
  let ptr' = parentPointer ptr
  fromIntegral <$> [C.exp| int { $(Texture* ptr')->GetHeight() } |]

textureDepth :: (Parent Texture a, Pointer p a, MonadIO m) => p -> m Int 
textureDepth ptr = liftIO $ do 
  let ptr' = parentPointer ptr
  fromIntegral <$> [C.exp| int { $(Texture* ptr')->GetDepth() } |]