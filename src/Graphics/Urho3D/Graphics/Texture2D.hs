{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.Texture2D(
    Texture2D
  , texture2DContext
  , texture2DSetSize
  , texture2DSetData
  , texture2DSetDataFromImage
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Graphics.Internal.Texture2D
import Graphics.Urho3D.Math.StringHash

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.Image
import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx
  <> texture2DCntx
  <> textureContext
  <> stringHashContext
  <> objectContext
  <> resourceContext
  <> imageContext)
C.include "<Urho3D/Graphics/Texture2D.h>"
C.using "namespace Urho3D"

texture2DContext :: C.Context
texture2DContext = texture2DCntx
  <> textureContext

instance ResourceType Texture2D where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Texture2D::GetTypeStatic().Value() } |]

deriveParents [''Object, ''Resource, ''Texture] ''Texture2D

-- | Set size, format, usage and multisampling parameters for rendertargets. Zero size will follow application window size. Return true if successful.
-- Autoresolve true means the multisampled texture will be automatically resolved to 1-sample after being rendered to and before being sampled as a texture.
-- Autoresolve false means the multisampled texture will be read as individual samples in the shader and is not supported on Direct3D9.
texture2DSetSize :: (Parent Texture2D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Texture2D or acenstor
  -> Int -- ^ width
  -> Int -- ^ height
  -> Word -- ^ format (?)
  -> TextureUsage -- ^ usate (default TextureStatic)
  -> Int -- ^ multi sample (default 1)
  -> Bool -- ^ auto resolve (default true)
  -> m Bool
texture2DSetSize p width height format usage multiSample autoResolve = liftIO $ do
  let ptr = parentPointer p
      width' = fromIntegral width
      height' = fromIntegral height
      format' = fromIntegral format
      usage' = fromIntegral . fromEnum $ usage
      multiSample' = fromIntegral multiSample
      autoResolve' = fromBool autoResolve
  toBool <$> [C.exp| int { (int)$(Texture2D* ptr)->SetSize($(int width'), $(int height'), $(unsigned int format'), (TextureUsage)$(int usage'), $(int multiSample'), $(int autoResolve') != 0) } |]

-- | Set data either partially or fully on a mip level. Return true if successful.
texture2DSetData :: (Parent Texture2D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Texture2D or acenstor
  -> Word -- ^ level
  -> Int -- ^ x
  -> Int -- ^ y
  -> Int -- ^ width
  -> Int -- ^ height
  -> Ptr () -- ^ data
  -> m Bool
texture2DSetData p level x y width height datum = liftIO $ do
  let ptr = parentPointer p
      level' = fromIntegral level
      x' = fromIntegral x
      y' = fromIntegral x
      width' = fromIntegral width
      height' = fromIntegral height
  toBool <$> [C.exp| int { (int)$(Texture2D* ptr)->SetData($(int level'), $(int x'), $(int y'), $(int width'), $(int height'), $(void* datum)) } |]

-- | Set data either partially or fully on a mip level. Return true if successful.
texture2DSetDataFromImage :: (Parent Texture2D a, Pointer ptr a, Parent Image b, Pointer pimage b, MonadIO m)
  => ptr -- ^ Pointer to Texture2D or acenstor
  -> pimage -- ^ Pointer to image
  -> Bool -- ^ use alpha (default False)
  -> m Bool
texture2DSetDataFromImage p pimage useAlpha = liftIO $ do
  let ptr = parentPointer p
      pimage' = parentPointer pimage
      useAlpha' = fromBool useAlpha
  toBool <$> [C.exp| int { (int)$(Texture2D* ptr)->SetData($(Image* pimage'), $(int useAlpha') != 0) } |]
