{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Viewport(
    Viewport
  , viewportContext
  , SharedViewport
  , viewportSetScene
  , viewportSetCamera
  , viewportSetRect
  , viewportSetRenderPath
  , viewportSetRenderPathXML
  , viewportSetDrawDebug
  , viewportSetCullCamera
  , viewportGetScene
  , viewportGetCamera
  , viewportGetView
  , viewportGetRect
  , viewportGetRenderPath
  , viewportGetDrawDebug
  , viewportGetCullCamera
  , viewportGetScreenRay
  , viewportWorldToScreenPoint
  , viewportScreenToWorldPoint
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad

import Graphics.Urho3D.Graphics.Internal.Viewport

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Camera
import Graphics.Urho3D.Graphics.RenderPath
import Graphics.Urho3D.Graphics.View
import Graphics.Urho3D.Math.Ray
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.XMLFile
import Graphics.Urho3D.Scene.Scene

C.context (C.cppCtx
  <> sharedViewportPtrCntx
  <> viewportCntx
  <> contextContext
  <> sceneContext
  <> cameraContext
  <> objectContext
  <> rectContext
  <> renderPathContext
  <> xmlFileContext
  <> viewContext
  <> rayContext
  <> vector2Context
  <> vector3Context
  )
C.include "<Urho3D/Graphics/Viewport.h>"
C.using "namespace Urho3D"

viewportContext :: C.Context
viewportContext = sharedViewportPtrCntx
  <> viewportCntx
  <> objectContext

instance Creatable (Ptr Viewport) where
  type CreationOptions (Ptr Viewport) = (Ptr Context, Ptr Scene, Ptr Camera)

  newObject (cntxPtr, scenePtr, camPtr) = liftIO $ [C.exp| Viewport* { new Viewport( $(Context* cntxPtr), $(Scene* scenePtr), $(Camera* camPtr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Viewport* ptr) } |]

deriveParent ''Object ''Viewport

sharedPtr "Viewport"

-- | Set scene.
-- void SetScene(Scene* scene);
viewportSetScene :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> Ptr Scene -> m ()
viewportSetScene ptr sptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| void { $(Viewport* ptr')->SetScene($(Scene* sptr)) } |]

-- | Set viewport camera.
-- void SetCamera(Camera* camera);
viewportSetCamera :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> Ptr Camera -> m ()
viewportSetCamera ptr cptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| void { $(Viewport* ptr')->SetCamera($(Camera* cptr)) } |]

-- | Set view rectangle. A zero rectangle (0 0 0 0) means to use the rendertarget's full dimensions.
-- void SetRect(const IntRect& rect);
viewportSetRect :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> IntRect -> m ()
viewportSetRect ptr r = liftIO $ with r $ \r' -> do
  let ptr' = parentPointer ptr
  [C.exp| void { $(Viewport* ptr')->SetRect(*$(IntRect* r')) } |]

-- | Set rendering path.
-- void SetRenderPath(RenderPath* renderPath);
viewportSetRenderPath :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> Ptr RenderPath -> m ()
viewportSetRenderPath ptr pptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| void { $(Viewport* ptr')->SetRenderPath($(RenderPath* pptr)) } |]

-- | Set rendering path from an XML file.
-- void SetRenderPath(XMLFile* file);
viewportSetRenderPathXML :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> Ptr XMLFile -> m ()
viewportSetRenderPathXML ptr fptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| void { $(Viewport* ptr')->SetRenderPath($(XMLFile* fptr)) } |]

-- | Set whether to render debug geometry. Default true.
-- void SetDrawDebug(bool enable);
viewportSetDrawDebug :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> Bool -> m ()
viewportSetDrawDebug ptr v = liftIO $ do
  let ptr' = parentPointer ptr
      v' = fromBool v
  [C.exp| void { $(Viewport* ptr')->SetDrawDebug($(int v') != 0) } |]

-- | Set separate camera to use for culling. Sharing a culling camera between several viewports allows to prepare the view only once, saving in CPU use. The culling camera's frustum should cover all the viewport cameras' frusta or else objects may be missing from the rendered view.
-- void SetCullCamera(Camera* camera);
viewportSetCullCamera :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> Ptr Camera -> m ()
viewportSetCullCamera ptr cptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| void { $(Viewport* ptr')->SetCullCamera($(Camera* cptr)) } |]

-- | Return scene.
-- Scene* GetScene() const;
viewportGetScene :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> m (Ptr Scene)
viewportGetScene ptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| Scene* { $(Viewport* ptr')->GetScene() } |]

-- | Return viewport camera.
-- Camera* GetCamera() const;
viewportGetCamera :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> m (Ptr Camera)
viewportGetCamera ptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| Camera* { $(Viewport* ptr')->GetCamera() } |]

-- | Return the internal rendering structure. May be null if the viewport has not been rendered yet.
-- View* GetView() const;
viewportGetView :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> m (Ptr View)
viewportGetView ptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| View* { $(Viewport* ptr')->GetView() } |]

-- | Return view rectangle. A zero rectangle (0 0 0 0) means to use the rendertarget's full dimensions. In this case you could fetch the actual view rectangle from View object, though it will be valid only after the first frame.
-- const IntRect& GetRect() const { return rect_; }
viewportGetRect :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> m IntRect
viewportGetRect ptr = liftIO $ do
  let ptr' = parentPointer ptr
  peek =<< [C.exp| const IntRect* { &$(Viewport* ptr')->GetRect() } |]

-- | Return rendering path.
-- RenderPath* GetRenderPath() const;
viewportGetRenderPath :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> m (Ptr RenderPath)
viewportGetRenderPath ptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| RenderPath* { $(Viewport* ptr')->GetRenderPath() } |]

-- | Return whether to draw debug geometry.
-- bool GetDrawDebug() const { return drawDebug_; }
viewportGetDrawDebug :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> m Bool
viewportGetDrawDebug ptr = liftIO $ do
  let ptr' = parentPointer ptr
  toBool <$> [C.exp| int { (int)$(Viewport* ptr')->GetDrawDebug() } |]

-- | Return the culling camera. If null, the viewport camera will be used for culling (normal case.)
-- Camera* GetCullCamera() const;
viewportGetCullCamera :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> m (Ptr Camera)
viewportGetCullCamera ptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| Camera* { $(Viewport* ptr')->GetCullCamera() } |]

-- | Return ray corresponding to normalized screen coordinates.
-- Ray GetScreenRay(int x, int y) const;
viewportGetScreenRay :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> Int -> Int -> m Ray
viewportGetScreenRay ptr px py = liftIO $ alloca $ \resptr -> do
  let ptr' = parentPointer ptr
      px' = fromIntegral px
      py' = fromIntegral py
  [C.exp| void {
    *($(Ray* resptr)) = $(Viewport* ptr')->GetScreenRay($(int px'), $(int py'))
    } |]
  peek resptr

-- | Convert a world space point to normalized screen coordinates.
-- IntVector2 WorldToScreenPoint(const Vector3& worldPos) const;
viewportWorldToScreenPoint :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> Vector3 -> m IntVector2
viewportWorldToScreenPoint ptr v = liftIO $ alloca $ \resptr -> with v $ \v' -> do
  let ptr' = parentPointer ptr
  [C.exp| void {
    *($(IntVector2* resptr)) = $(Viewport* ptr')->WorldToScreenPoint(*$(Vector3* v'))
    } |]
  peek resptr

-- | Convert screen coordinates and depth to a world space point.
-- Vector3 ScreenToWorldPoint(int x, int y, float depth) const;
viewportScreenToWorldPoint :: (Parent Viewport a, Pointer p a, MonadIO m) => p -> Int -> Int -> Float -> m Vector3
viewportScreenToWorldPoint ptr px py d = liftIO $ alloca $ \resptr -> do
  let ptr' = parentPointer ptr
      px' = fromIntegral px
      py' = fromIntegral py
      d' = realToFrac d
  [C.exp| void {
    *($(Vector3* resptr)) = $(Viewport* ptr')->ScreenToWorldPoint($(int px'), $(int py'), $(float d'))
    } |]
  peek resptr
