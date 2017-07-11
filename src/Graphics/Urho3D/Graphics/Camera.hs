{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Camera(
    Camera
  , cameraContext
  , cameraSetNearClip
  , cameraSetFarClip
  , cameraSetFov
  , cameraSetOrthoSize
  , cameraSetOrthoSizeVec
  , cameraSetAspectRatio
  , cameraSetFillMode
  , cameraSetZoom
  , cameraSetLodBias
  , cameraSetViewMask
  , cameraSetViewOverrideFlags
  , cameraSetOrthographic
  , cameraSetAutoAspectRatio
  , cameraSetProjectionOffset
  , cameraSetUseReflection
  , cameraSetReflectionPlane
  , cameraSetUseClipping
  , cameraSetClipPlane
  , cameraSetFlipVertical
  , cameraSetProjection
  , cameraGetFarClip
  , cameraGetNearClip
  , cameraGetFov
  , cameraGetOrthoSize
  , cameraGetAspectRatio
  , cameraGetZoom
  , cameraGetLodBias
  , cameraGetViewMask
  , cameraGetViewOverrideFlags
  , cameraGetFillMode
  , cameraIsOrthographic
  , cameraGetAutoAspectRatio
  , cameraGetFrustum
  , cameraGetProjection
  , cameraGetGPUProjection
  , cameraGetView
  , cameraGetFrustumSize
  , cameraGetHalfViewSize
  , cameraGetSplitFrustum
  , cameraGetViewSpaceFrustum
  , cameraGetViewSpaceSplitFrustum
  , cameraGetScreenRay
  , cameraWorldToScreenPoint
  , cameraScreenToWorldPoint
  , cameraGetProjectionOffset
  , cameraGetUseReflection
  , cameraGetReflectionPlane
  , cameraGetUseClipping
  , cameraGetClipPlane
  , cameraGetFlipVertical
  , cameraGetReverseCulling
  , cameraGetDistance
  , cameraGetDistanceSquared
  , cameraGetLodDistance
  , cameraGetFaceCameraRotation
  , cameraGetEffectiveWorldTransform
  , cameraIsProjectionValid
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Graphics.Internal.Camera
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Scene.Node
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Math.Frustum
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Math.Matrix4
import Graphics.Urho3D.Math.Plane
import Graphics.Urho3D.Math.Quaternion
import Graphics.Urho3D.Math.Ray
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx
  <> cameraCntx
  <> componentContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> vector2Context
  <> vector3Context
  <> planeContext
  <> quaternionContext
  <> frustumContext
  <> matrix3x4Context
  <> matrix4Context
  <> rayContext
  )
C.include "<Urho3D/Graphics/Camera.h>"
C.using "namespace Urho3D"

cameraContext :: C.Context
cameraContext = componentContext <> cameraCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''Component] ''Camera

instance NodeComponent Camera where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Camera::GetTypeStatic().Value() } |]

-- | Set near clip distance.
cameraSetNearClip :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ Value of near clip plain
  -> m ()
cameraSetNearClip p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetNearClip($(float v'))} |]

-- | Set far clip distance.
cameraSetFarClip :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ Value of far clip plain
  -> m ()
cameraSetFarClip p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetFarClip($(float v'))} |]

-- | Set vertical field of view in degrees.
cameraSetFov :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ fov value
  -> m ()
cameraSetFov p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetFov($(float v'))} |]

-- | Set orthographic mode view uniform size.
-- void SetOrthoSize(float orthoSize);
cameraSetOrthoSize :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ orthoSize
  -> m ()
cameraSetOrthoSize p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetOrthoSize($(float v'))} |]

-- | Set orthographic mode view non-uniform size. Disables the auto aspect ratio -mode.
-- void SetOrthoSize(const Vector2& orthoSize);
cameraSetOrthoSizeVec :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Vector2 -- ^ orthoSize
  -> m ()
cameraSetOrthoSizeVec p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Camera* ptr)->SetOrthoSize(*$(Vector2* v'))} |]

-- | Set aspect ratio manually. Disables the auto aspect ratio -mode.
-- void SetAspectRatio(float aspectRatio);
cameraSetAspectRatio :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ aspectRatio
  -> m ()
cameraSetAspectRatio p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetAspectRatio($(float v'))} |]

-- | Set polygon fill mode to use when rendering a scene.
-- void SetFillMode(FillMode mode);
cameraSetFillMode :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> FillMode -- ^ mode
  -> m ()
cameraSetFillMode p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral . fromEnum $ v
  [C.exp| void {$(Camera* ptr)->SetFillMode((FillMode)$(int v'))} |]

-- | Set zoom.
-- void SetZoom(float zoom);
cameraSetZoom :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ zoom
  -> m ()
cameraSetZoom p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetZoom($(float v'))} |]

-- | Set LOD bias.
-- void SetLodBias(float bias);
cameraSetLodBias :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Float -- ^ bias
  -> m ()
cameraSetLodBias p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Camera* ptr)->SetLodBias($(float v'))} |]

-- | Set view mask. Will be and'ed with object's view mask to see if the object should be rendered.
-- void SetViewMask(unsigned mask);
cameraSetViewMask :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Word -- ^ mask
  -> m ()
cameraSetViewMask p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void {$(Camera* ptr)->SetViewMask($(unsigned int v'))} |]

-- | Set view override flags.
-- void SetViewOverrideFlags(unsigned flags);
cameraSetViewOverrideFlags :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Word -- ^ flags
  -> m ()
cameraSetViewOverrideFlags p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void {$(Camera* ptr)->SetViewOverrideFlags($(unsigned int v'))} |]

-- | Set orthographic mode enabled/disabled.
-- void SetOrthographic(bool enable);
cameraSetOrthographic :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Bool -- ^ enable
  -> m ()
cameraSetOrthographic p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(Camera* ptr)->SetOrthographic($(int v') != 0)} |]

-- | Set automatic aspect ratio based on viewport dimensions. Enabled by default.
-- void SetAutoAspectRatio(bool enable);
cameraSetAutoAspectRatio :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Bool -- ^ enable
  -> m ()
cameraSetAutoAspectRatio p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(Camera* ptr)->SetAutoAspectRatio($(int v') != 0)} |]

-- | Set projection offset. It needs to be calculated as (offset in pixels) / (viewport dimensions.)
-- void SetProjectionOffset(const Vector2& offset);
cameraSetProjectionOffset :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Vector2 -- ^ offset
  -> m ()
cameraSetProjectionOffset p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Camera* ptr)->SetProjectionOffset(*$(Vector2* v'))} |]

-- | Set reflection mode.
-- void SetUseReflection(bool enable);
cameraSetUseReflection :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Bool -- ^ enable
  -> m ()
cameraSetUseReflection p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(Camera* ptr)->SetUseReflection($(int v') != 0)} |]

-- | Set reflection plane in world space for reflection mode.
-- void SetReflectionPlane(const Plane& plane);
cameraSetReflectionPlane :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Plane -- ^ plane
  -> m ()
cameraSetReflectionPlane p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Camera* ptr)->SetReflectionPlane(*$(Plane* v'))} |]

-- | Set whether to use a custom clip plane.
-- void SetUseClipping(bool enable);
cameraSetUseClipping :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Bool -- ^ enable
  -> m ()
cameraSetUseClipping p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(Camera* ptr)->SetUseClipping($(int v') != 0)} |]

-- | Set custom clipping plane in world space.
-- void SetClipPlane(const Plane& plane);
cameraSetClipPlane :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Plane -- ^ plane
  -> m ()
cameraSetClipPlane p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Camera* ptr)->SetClipPlane(*$(Plane* v'))} |]

-- | Set vertical flipping mode. Called internally by View to resolve OpenGL / Direct3D9 rendertarget sampling differences.
-- void SetFlipVertical(bool enable);
cameraSetFlipVertical :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Bool -- ^ enable
  -> m ()
cameraSetFlipVertical p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(Camera* ptr)->SetFlipVertical($(int v') != 0)} |]

-- | Set custom projection matrix, which should be specified in D3D convention with depth range 0 - 1. Disables auto aspect ratio.
-- Change any of the standard view parameters (FOV, far clip, zoom etc.) to revert to the standard projection.
-- Note that the custom projection is not serialized or replicated through the network.
-- void SetProjection(const Matrix4& projection);
cameraSetProjection :: (Parent Camera a, Pointer p a, MonadIO m)
  => p -- ^ Camera pointer or child
  -> Matrix4 -- ^ projection
  -> m ()
cameraSetProjection p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Camera* ptr)->SetProjection(*$(Matrix4* v'))} |]

-- | Return far clip distance. If a custom projection matrix is in use, is calculated from it instead of the value assigned with SetFarClip().
-- float GetFarClip() const;
cameraGetFarClip :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetFarClip p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetFarClip()} |]

-- | Return near clip distance. If a custom projection matrix is in use, is calculated from it instead of the value assigned with SetNearClip().
-- float GetNearClip() const;
cameraGetNearClip :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetNearClip p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetNearClip()} |]

-- | Return vertical field of view in degrees.
cameraGetFov :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetFov p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetFov()} |]

-- | Return orthographic mode size.
-- float GetOrthoSize() const { return orthoSize_; }
cameraGetOrthoSize :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetOrthoSize p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetOrthoSize()} |]

-- | Return aspect ratio.
-- float GetAspectRatio() const { return aspectRatio_; }
cameraGetAspectRatio :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetAspectRatio p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetAspectRatio()} |]

-- | Return zoom.
-- float GetZoom() const { return zoom_; }
cameraGetZoom :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetZoom p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetZoom()} |]

-- | Return LOD bias.
-- float GetLodBias() const { return lodBias_; }
cameraGetLodBias :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetLodBias p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetLodBias()} |]

-- | Return view mask.
-- unsigned GetViewMask() const { return viewMask_; }
cameraGetViewMask :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Word
cameraGetViewMask p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Camera* ptr)->GetViewMask()} |]

-- | Return view override flags.
-- unsigned GetViewOverrideFlags() const { return viewOverrideFlags_; }
cameraGetViewOverrideFlags :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Word
cameraGetViewOverrideFlags p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Camera* ptr)->GetViewOverrideFlags()} |]

-- | Return fill mode.
-- FillMode GetFillMode() const { return fillMode_; }
cameraGetFillMode :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m FillMode
cameraGetFillMode p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(Camera* ptr)->GetFillMode()} |]

-- | Return orthographic flag.
-- bool IsOrthographic() const { return orthographic_; }
cameraIsOrthographic :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Bool
cameraIsOrthographic p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Camera* ptr)->IsOrthographic()} |]

-- | Return auto aspect ratio flag.
-- bool GetAutoAspectRatio() const { return autoAspectRatio_; }
cameraGetAutoAspectRatio :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Bool
cameraGetAutoAspectRatio p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Camera* ptr)->GetAutoAspectRatio()} |]

-- | Return frustum in world space.
-- const Frustum& GetFrustum() const;
cameraGetFrustum :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Frustum
cameraGetFrustum p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Frustum* {&$(Camera* ptr)->GetFrustum()} |]

-- | Return projection matrix. It's in D3D convention with depth range 0 - 1.
-- Matrix4 GetProjection() const;
cameraGetProjection :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Matrix4
cameraGetProjection p = liftIO $ alloca $ \rptr -> do
  let ptr = parentPointer p
  [C.exp| void {*$(Matrix4* rptr) = $(Camera* ptr)->GetProjection()} |]
  peek rptr

-- | Return projection matrix converted to API-specific format for use as a shader parameter.
-- Matrix4 GetGPUProjection() const;
cameraGetGPUProjection :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Matrix4
cameraGetGPUProjection p = liftIO $ alloca $ \rptr -> do
  let ptr = parentPointer p
  [C.exp| void {*$(Matrix4* rptr) = $(Camera* ptr)->GetGPUProjection()} |]
  peek rptr

-- | Return view matrix.
-- const Matrix3x4& GetView() const;
cameraGetView :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Matrix3x4
cameraGetView p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Matrix3x4* {&$(Camera* ptr)->GetView()} |]

-- | Return frustum near and far sizes.
-- void GetFrustumSize(Vector3& near, Vector3& far) const;
cameraGetFrustumSize :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m (Vector3, Vector3)
cameraGetFrustumSize p = liftIO $ alloca $ \nearp -> alloca $ \farp -> do
  let ptr = parentPointer p
  [C.exp| void { $(Camera* ptr)->GetFrustumSize(*$(Vector3* nearp), *$(Vector3* farp)) } |]
  (,) <$> peek nearp <*> peek farp

-- | Return half view size.
-- float GetHalfViewSize() const;
cameraGetHalfViewSize :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Float
cameraGetHalfViewSize p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetHalfViewSize()} |]

-- | Return frustum split by custom near and far clip distances.
-- Frustum GetSplitFrustum(float nearClip, float farClip) const;
cameraGetSplitFrustum :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> Float -- ^ near clip
  -> Float -- ^ far clip
  -> m Frustum
cameraGetSplitFrustum p nearv farv = liftIO $ alloca $ \frp -> do
  let ptr = parentPointer p
      nearv' = realToFrac nearv
      farv' = realToFrac farv
  [C.exp| void {*$(Frustum* frp) = $(Camera* ptr)->GetSplitFrustum($(float nearv'), $(float farv'))} |]
  peek frp

-- | Return frustum in view space.
-- Frustum GetViewSpaceFrustum() const;
cameraGetViewSpaceFrustum :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Frustum
cameraGetViewSpaceFrustum p = liftIO $ alloca $ \frp -> do
  let ptr = parentPointer p
  [C.exp| void {*$(Frustum* frp) = $(Camera* ptr)->GetViewSpaceFrustum()} |]
  peek frp

-- | Return split frustum in view space.
-- Frustum GetViewSpaceSplitFrustum(float nearClip, float farClip) const;
cameraGetViewSpaceSplitFrustum :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> Float -- ^ near clip
  -> Float -- ^ far clip
  -> m Frustum
cameraGetViewSpaceSplitFrustum p nearv farv = liftIO $ alloca $ \frp -> do
  let ptr = parentPointer p
      nearv' = realToFrac nearv
      farv' = realToFrac farv
  [C.exp| void {*$(Frustum* frp) = $(Camera* ptr)->GetViewSpaceSplitFrustum($(float nearv'), $(float farv'))} |]
  peek frp

-- | Return ray corresponding to normalized screen coordinates (0 - 1), with origin on the near clip plane.
-- Ray GetScreenRay(float x, float y) const;
cameraGetScreenRay :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> Float -- ^ x
  -> Float -- ^ y
  -> m Ray
cameraGetScreenRay p rx ry = liftIO $ alloca $ \aptr -> do
  let ptr = parentPointer p
      rx' = realToFrac rx
      ry' = realToFrac ry
  [C.exp| void {*$(Ray* aptr) = $(Camera* ptr)->GetScreenRay($(float rx'), $(float ry'))} |]
  peek aptr

-- | Convert a world space point to normalized screen coordinates (0 - 1).
-- Vector2 WorldToScreenPoint(const Vector3& worldPos) const;
cameraWorldToScreenPoint :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> Vector3 -- ^ World pos
  -> m Vector2
cameraWorldToScreenPoint p v = liftIO $ with v $ \v' -> alloca $ \vp -> do
  let ptr = parentPointer p
  [C.exp| void {*$(Vector2* vp) = $(Camera* ptr)->WorldToScreenPoint(*$(Vector3* v'))} |]
  peek vp

-- | Convert normalized screen coordinates (0 - 1) and distance along view Z axis (in Z coordinate) to a world space point. The distance can not be closer than the near clip plane.
-- Note that a HitDistance() from the camera screen ray is not the same as distance along the view Z axis, as under a perspective projection the ray is likely to not be Z-aligned.
-- Vector3 ScreenToWorldPoint(const Vector3& screenPos) const;
cameraScreenToWorldPoint :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> Vector3 -- ^ Sceen pos
  -> m Vector3
cameraScreenToWorldPoint p v = liftIO $ with v $ \v' -> alloca $ \vp -> do
  let ptr = parentPointer p
  [C.exp| void {*$(Vector3* vp) = $(Camera* ptr)->ScreenToWorldPoint(*$(Vector3* v'))} |]
  peek vp

-- | Return projection offset.
-- const Vector2& GetProjectionOffset() const { return projectionOffset_; }
cameraGetProjectionOffset :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Vector2
cameraGetProjectionOffset p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Vector2* {&$(Camera* ptr)->GetProjectionOffset()} |]

-- | Return whether is using reflection.
-- bool GetUseReflection() const { return useReflection_; }
cameraGetUseReflection :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Bool
cameraGetUseReflection p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Camera* ptr)->GetUseReflection()} |]

-- | Return the reflection plane.
-- const Plane& GetReflectionPlane() const { return reflectionPlane_; }
cameraGetReflectionPlane :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Plane
cameraGetReflectionPlane p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Plane* {&$(Camera* ptr)->GetReflectionPlane()} |]

-- | Return whether is using a custom clipping plane.
-- bool GetUseClipping() const { return useClipping_; }
cameraGetUseClipping :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Bool
cameraGetUseClipping p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Camera* ptr)->GetUseClipping()} |]

-- | Return the custom clipping plane.
-- const Plane& GetClipPlane() const { return clipPlane_; }
cameraGetClipPlane :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Plane
cameraGetClipPlane p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Plane* {&$(Camera* ptr)->GetClipPlane()} |]

-- | Return vertical flipping mode.
-- bool GetFlipVertical() const { return flipVertical_; }
cameraGetFlipVertical :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Bool
cameraGetFlipVertical p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Camera* ptr)->GetFlipVertical()} |]

-- | Return whether to reverse culling; affected by vertical flipping and reflection.
-- bool GetReverseCulling() const { return flipVertical_ ^ useReflection_; }
cameraGetReverseCulling :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Bool
cameraGetReverseCulling p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Camera* ptr)->GetReverseCulling()} |]

-- | Return distance to position. In orthographic mode uses only Z coordinate.
-- float GetDistance(const Vector3& worldPos) const;
cameraGetDistance :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> Vector3 -- ^ World pos
  -> m Float
cameraGetDistance p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetDistance(*$(Vector3* v'))} |]

-- | Return squared distance to position. In orthographic mode uses only Z coordinate.
-- float GetDistanceSquared(const Vector3& worldPos) const;
cameraGetDistanceSquared :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> Vector3 -- ^ World pos
  -> m Float
cameraGetDistanceSquared p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetDistanceSquared(*$(Vector3* v'))} |]

-- | Return a scene node's LOD scaled distance.
-- float GetLodDistance(float distance, float scale, float bias) const;
cameraGetLodDistance :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> Float -- ^ distance
  -> Float -- ^ scale
  -> Float -- ^ bias
  -> m Float
cameraGetLodDistance p distance scale bias = liftIO $ do
  let ptr = parentPointer p
      distance' = realToFrac distance
      scale' = realToFrac scale
      bias' = realToFrac bias
  realToFrac <$> [C.exp| float {$(Camera* ptr)->GetLodDistance($(float distance'), $(float scale'), $(float bias'))} |]

-- | Return a world rotation for facing a camera on certain axes based on the existing world rotation.
-- Quaternion GetFaceCameraRotation(const Vector3& position, const Quaternion& rotation, FaceCameraMode mode, float minAngle = 0.0f);
cameraGetFaceCameraRotation :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> Vector3 -- ^ position
  -> Quaternion -- ^ rotation
  -> FaceCameraMode -- ^ mode
  -> Float -- ^ min angle (default 0)
  -> m Quaternion
cameraGetFaceCameraRotation p pos rot mode minAngle = liftIO $ with pos $ \pos' -> with rot $ \rot' -> alloca $ \qp -> do
  let ptr = parentPointer p
      mode' = fromIntegral . fromEnum $ mode
      minAngle' = realToFrac minAngle
  [C.exp| void {*$(Quaternion* qp) = $(Camera* ptr)->GetFaceCameraRotation(*$(Vector3* pos'), *$(Quaternion* rot'), (FaceCameraMode)$(int mode'), $(float minAngle'))} |]
  peek qp

-- | Get effective world transform for matrix and frustum calculations including reflection but excluding node scaling.
-- Matrix3x4 GetEffectiveWorldTransform() const;
cameraGetEffectiveWorldTransform :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Matrix3x4
cameraGetEffectiveWorldTransform p = liftIO $ alloca $ \rptr -> do
  let ptr = parentPointer p
  [C.exp| void {*$(Matrix3x4* rptr) = $(Camera* ptr)->GetEffectiveWorldTransform()} |]
  peek rptr

-- | Return if projection parameters are valid for rendering and raycasting.
-- bool IsProjectionValid() const;
cameraIsProjectionValid :: (Parent Camera a, Pointer p a, MonadIO m) => p -- ^ Camera pointer or child
  -> m Bool
cameraIsProjectionValid p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Camera* ptr)->IsProjectionValid()} |]
