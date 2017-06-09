{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.Graphics(
    Graphics
  , graphicsContext
  , graphicsSetWindowIcon
  , graphicsSetWindowTitle
  , graphicsTakeScreenShot
  , graphicsGetHeight
  , graphicsGetWidth
  , getAlphaFormat
  , getLuminanceFormat
  , getLuminanceAlphaFormat
  , getRGBFormat
  , getRGBAFormat
  , getRGBA16Format
  , getRGBAFloat16Format
  , getRGBAFloat32Format
  , getRG16Format
  , getRGFloat16Format
  , getRGFloat32Format
  , getFloat16Format
  , getFloat32Format
  , getLinearDepthFormat
  , getDepthStencilFormat
  , getReadableDepthFormat
  , getFormat
  , getPixelUVOffset
  , getMaxBones
  , getGL3Support
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Foreign.C.String
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Internal.Graphics
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.Image
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> graphicsCntx <> objectContext <> imageContext <> vector2Context)
C.include "<Urho3D/Graphics/Graphics.h>"
C.using "namespace Urho3D"

graphicsContext :: C.Context
graphicsContext = objectContext <> graphicsCntx

deriveParent ''Object ''Graphics

instance Subsystem Graphics where
  getSubsystemImpl ptr = [C.exp| Graphics* { $(Object* ptr)->GetSubsystem<Graphics>() } |]

-- | Sets current window icon
graphicsSetWindowIcon :: (Parent Graphics a, Pointer p a, MonadIO m) => p -- ^ Pointer to graphics system or child
  -> Ptr Image -- ^ Pointer to icon resource
  -> m ()
graphicsSetWindowIcon ptr icon = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| void { $(Graphics* ptr')->SetWindowIcon($(Image* icon)) }|]

-- | Sets current window title
graphicsSetWindowTitle :: (Parent Graphics a, Pointer p a, MonadIO m) => p -- ^ Pointer to graphics system or child
  -> String -- ^ Title
  -> m ()
graphicsSetWindowTitle ptr str = liftIO $ withCString str $ \str' -> do
  let ptr' = parentPointer ptr
  [C.exp| void { $(Graphics* ptr')->SetWindowTitle(String($(const char* str'))) }|]

-- | Takes screenshot and writes data in image
graphicsTakeScreenShot :: (Parent Graphics a, Pointer p a, MonadIO m) => p -- ^ Pointer to graphics system or child
  -> Ptr Image -- ^ Pointer to image where to save data
  -> m ()
graphicsTakeScreenShot p img = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Graphics* ptr)->TakeScreenShot(*$(Image* img)) } |]

-- | Returns window height
graphicsGetHeight :: (Parent Graphics a, Pointer p a, MonadIO m) => p -- ^ Pointer to graphics system or child
  -> m Int
graphicsGetHeight p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(Graphics* ptr)->GetHeight() } |]

-- | Returns window width
graphicsGetWidth :: (Parent Graphics a, Pointer p a, MonadIO m) => p -- ^ Pointer to graphics system or child
  -> m Int
graphicsGetWidth p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(Graphics* ptr)->GetWidth() } |]

-- | Return the API-specific alpha texture format.
getAlphaFormat :: Word
getAlphaFormat = fromIntegral [C.pure| unsigned int { Graphics::GetAlphaFormat() } |]

-- | Return the API-specific luminance texture format.
getLuminanceFormat :: Word
getLuminanceFormat = fromIntegral [C.pure| unsigned int { Graphics::GetLuminanceFormat() } |]

-- | Return the API-specific luminance alpha texture format.
getLuminanceAlphaFormat :: Word
getLuminanceAlphaFormat = fromIntegral [C.pure| unsigned int { Graphics::GetLuminanceAlphaFormat() } |]

-- | Return the API-specific RGB texture format.
getRGBFormat :: Word
getRGBFormat = fromIntegral [C.pure| unsigned int { Graphics::GetRGBFormat() } |]

-- | Return the API-specific RGBA texture format.
getRGBAFormat :: Word
getRGBAFormat = fromIntegral [C.pure| unsigned int { Graphics::GetRGBAFormat() } |]

-- | Return the API-specific RGBA 16-bit texture format.
getRGBA16Format :: Word
getRGBA16Format = fromIntegral [C.pure| unsigned int { Graphics::GetRGBA16Format() } |]

-- | Return the API-specific RGBA 16-bit float texture format.
getRGBAFloat16Format :: Word
getRGBAFloat16Format = fromIntegral [C.pure| unsigned int { Graphics::GetRGBAFloat16Format() } |]

-- | Return the API-specific RGBA 32-bit float texture format.
getRGBAFloat32Format :: Word
getRGBAFloat32Format = fromIntegral [C.pure| unsigned int { Graphics::GetRGBAFloat32Format() } |]

-- | Return the API-specific RG 16-bit texture format.
getRG16Format :: Word
getRG16Format = fromIntegral [C.pure| unsigned int { Graphics::GetRG16Format() } |]

-- | Return the API-specific RG 16-bit float texture format.
getRGFloat16Format :: Word
getRGFloat16Format = fromIntegral [C.pure| unsigned int { Graphics::GetRGFloat16Format() } |]

-- | Return the API-specific RG 32-bit float texture format.
getRGFloat32Format :: Word
getRGFloat32Format = fromIntegral [C.pure| unsigned int { Graphics::GetRGFloat32Format() } |]

-- | Return the API-specific single channel 16-bit float texture format.
getFloat16Format :: Word
getFloat16Format = fromIntegral [C.pure| unsigned int { Graphics::GetFloat16Format() } |]

-- | Return the API-specific single channel 32-bit float texture format.
getFloat32Format :: Word
getFloat32Format = fromIntegral [C.pure| unsigned int { Graphics::GetFloat32Format() } |]

-- | Return the API-specific linear depth texture format.
getLinearDepthFormat :: Word
getLinearDepthFormat = fromIntegral [C.pure| unsigned int { Graphics::GetLinearDepthFormat() } |]

-- | Return the API-specific hardware depth-stencil texture format.
getDepthStencilFormat :: Word
getDepthStencilFormat = fromIntegral [C.pure| unsigned int { Graphics::GetDepthStencilFormat() } |]

-- | Return the API-specific readable hardware depth format, or 0 if not supported.
getReadableDepthFormat :: Word
getReadableDepthFormat = fromIntegral [C.pure| unsigned int { Graphics::GetReadableDepthFormat() } |]

-- | Return the API-specific texture format from a textual description, for example "rgb".
getFormat :: String -> Word
getFormat formatName = unsafePerformIO $ withCString formatName $ \formatName' ->
  fromIntegral <$> [C.exp| unsigned int { Graphics::GetFormat(String($(const char* formatName'))) } |]
{-# NOINLINE getFormat #-}

-- | Return UV offset required for pixel perfect rendering.
getPixelUVOffset :: Vector2
getPixelUVOffset = unsafePerformIO $ peek =<< [C.exp| const Vector2* { &Graphics::GetPixelUVOffset() } |]
{-# NOINLINE getPixelUVOffset #-}

-- | Return maximum number of supported bones for skinning.
getMaxBones :: Word
getMaxBones = fromIntegral [C.pure| unsigned int { Graphics::GetMaxBones() } |]

-- | Return whether is using an OpenGL 3 context. Return always false on Direct3D9 & Direct3D11.
getGL3Support :: Bool
getGL3Support = toBool [C.pure| int { (int)Graphics::GetGL3Support() } |]
