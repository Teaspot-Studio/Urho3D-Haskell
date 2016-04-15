{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Graphics(
    Graphics
  , graphicsContext
  , graphicsSetWindowIcon
  , graphicsSetWindowTitle
  , graphicsTakeScreenShot
  , graphicsGetHeight
  , graphicsGetWidth
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Graphics
import Graphics.Urho3D.Resource.Image
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Data.Monoid
import Foreign 
import Foreign.C.String 

C.context (C.cppCtx <> graphicsCntx <> objectContext <> imageContext)
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