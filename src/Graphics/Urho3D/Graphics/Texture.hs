{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.Texture(
    Texture
  , textureContext
  , textureLevels
  , textureWidth
  , textureHeight
  , textureDepth
  , textureSetNumLevels
  , textureSetFilterMode
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Defs
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

-- | Set number of requested mip levels. Needs to be called before setting size.
-- The default value (0) allocates as many mip levels as necessary to reach 1x1 size. Set value 1 to disable mipmapping.
-- Note that rendertargets need to regenerate mips dynamically after rendering, which may cost performance. Screen buffers
-- and shadow maps allocated by Renderer will have mipmaps disabled.
textureSetNumLevels :: (Parent Texture a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to texture or ancestor
  -> Word -- ^ levels
  -> m ()
textureSetNumLevels ptr levels = liftIO $ do
  let ptr' = parentPointer ptr
      levels' = fromIntegral levels
  [C.exp| void { $(Texture* ptr')->SetNumLevels($(unsigned int levels')) } |]

-- | Set filtering mode.
textureSetFilterMode :: (Parent Texture a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to texture or ancestor
  -> TextureFilterMode -- ^ filter
  -> m ()
textureSetFilterMode ptr mode = liftIO $ do
  let ptr' = parentPointer ptr
      mode' = fromIntegral . fromEnum $ mode
  [C.exp| void { $(Texture* ptr')->SetFilterMode((TextureFilterMode)$(int mode')) } |]
