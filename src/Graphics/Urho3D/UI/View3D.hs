{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.View3D(
    View3D
  , view3DContext
  , SharedView3D
  , WeakView3D
  , view3DSetView
  , view3DSetFormat
  , view3DSetAutoUpdate
  , view3DQueueUpdate
  , view3DGetFormat
  , view3DGetAutoUpdate
  , view3DGetScene
  , view3DGetCameraNode
  , view3DGetRenderTexture
  , view3DGetDepthTexture
  , view3DGetViewport
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.View3D
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Camera
import Graphics.Urho3D.Graphics.Texture2D
import Graphics.Urho3D.Graphics.Viewport
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Node
import Graphics.Urho3D.Scene.Scene
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.Window

C.context (C.cppCtx
  <> sharedView3DPtrCntx
  <> weakView3DPtrCntx
  <> view3DCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> windowContext
  <> sceneContext
  <> cameraContext
  <> texture2DContext
  <> viewportContext
  <> nodeContext
  )
C.include "<Urho3D/UI/View3D.h>"
C.using "namespace Urho3D"

view3DContext :: C.Context
view3DContext = sharedView3DPtrCntx
  <> weakView3DPtrCntx
  <> view3DCntx

instance Creatable (Ptr View3D) where
  type CreationOptions (Ptr View3D) = Ptr Context

  newObject ptr = liftIO $ [C.exp| View3D* { new View3D( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(View3D* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage, ''Window] ''View3D

instance UIElem View3D where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { View3D::GetTypeStatic().Value() } |]

sharedPtr "View3D"
sharedWeakPtr "View3D"

-- | Define the scene and camera to use in rendering. When ownScene is true the
-- View3D will take ownership of them with shared pointers.
-- void SetView(Scene* scene, Camera* camera, bool ownScene = true);
view3DSetView :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> Ptr Scene
  -> Ptr Camera
  -> Bool -- ^ own scene (default True)
  -> m ()
view3DSetView p s cam ownScene = liftIO $ do
  let ptr = parentPointer p
      ownScene' = fromBool ownScene
  [C.exp| void { $(View3D* ptr)->SetView($(Scene* s), $(Camera* cam), $(int ownScene') != 0) } |]

-- | Set render texture pixel format. Default is RGB.
-- void SetFormat(unsigned format);
view3DSetFormat :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> Word -- ^ format
  -> m ()
view3DSetFormat p f = liftIO $ do
  let ptr = parentPointer p
      f' = fromIntegral f
  [C.exp| void { $(View3D* ptr)->SetFormat($(unsigned int f')) } |]

-- | Set render target auto update mode. Default is true.
-- void SetAutoUpdate(bool enable);
view3DSetAutoUpdate :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> Bool -- ^ enable
  -> m ()
view3DSetAutoUpdate p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(View3D* ptr)->SetAutoUpdate($(int v') != 0) } |]

-- | Queue manual update on the render texture.
-- void QueueUpdate();
view3DQueueUpdate :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> m ()
view3DQueueUpdate p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(View3D* ptr)->QueueUpdate() } |]

-- | Return render texture pixel format.
-- unsigned GetFormat() const { return rttFormat_; }
view3DGetFormat :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> m Word
view3DGetFormat p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(View3D* ptr)->GetFormat() } |]

-- | Return whether render target updates automatically.
-- bool GetAutoUpdate() const { return autoUpdate_; }
view3DGetAutoUpdate :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> m Bool
view3DGetAutoUpdate p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(View3D* ptr)->GetAutoUpdate() } |]

-- | Return scene.
-- Scene* GetScene() const;
view3DGetScene :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> m (Ptr Scene)
view3DGetScene p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Scene* { $(View3D* ptr)->GetScene() } |]

-- | Return camera scene node.
-- Node* GetCameraNode() const;
view3DGetCameraNode :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> m (Ptr Node)
view3DGetCameraNode p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Node* { $(View3D* ptr)->GetCameraNode() } |]

-- | Return render texture.
-- Texture2D* GetRenderTexture() const;
view3DGetRenderTexture :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> m (Ptr Texture2D)
view3DGetRenderTexture p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Texture2D* { $(View3D* ptr)->GetRenderTexture() } |]

-- | Return depth stencil texture.
-- Texture2D* GetDepthTexture() const;
view3DGetDepthTexture :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> m (Ptr Texture2D)
view3DGetDepthTexture p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Texture2D* { $(View3D* ptr)->GetDepthTexture() } |]

-- | Return viewport.
-- Viewport* GetViewport() const;
view3DGetViewport :: (Parent View3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'View3D' or ascentor
  -> m (Ptr Viewport)
view3DGetViewport p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Viewport* { $(View3D* ptr)->GetViewport() } |]
