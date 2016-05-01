{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.DebugRenderer(
    DebugRenderer
  , debugRendererContext
  , debugRendererSetView
  , DebugRendererAddLine(..)
  , DebugRendererAddTriangle(..)
  , debugRendererAddNode
  , debugRendererAddBoundingBox
  , debugRendererAddBoundingBoxWithTransform
  , debugRendererAddFrustum
  , debugRendererAddPolyhedron
  , debugRendererAddSphere
  , debugRendererAddCylinder
  , debugRendererAddSkeleton
  , debugRendererAddTriangleMesh
  , debugRendererAddCircle
  , debugRendererAddCross
  , debugRendererAddQuad
  , debugRendererRender
  , debugRendererGetView
  , debugRendererGetProjection
  , debugRendererGetFrustum
  , debugRendererIsInside
  , debugRendererHasContent
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.DebugRenderer
import Graphics.Urho3D.Scene.Node 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Graphics.Camera
import Graphics.Urho3D.Graphics.Skeleton
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.Frustum
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Math.Matrix4
import Graphics.Urho3D.Math.Polyhedron
import Graphics.Urho3D.Math.Sphere
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx 
  <> debugRendererCntx 
  <> componentContext 
  <> animatableContext 
  <> serializableContext 
  <> objectContext
  <> contextContext 
  <> cameraContext 
  <> colorContext 
  <> nodeContext 
  <> boundingBoxContext
  <> matrix3x4Context 
  <> matrix4Context
  <> frustumContext 
  <> polyhedronContext
  <> vector3Context
  <> sphereContext
  <> skeletonContext )

C.include "<Urho3D/Graphics/DebugRenderer.h>"
C.using "namespace Urho3D"

debugRendererContext :: C.Context 
debugRendererContext = componentContext 
  <> debugRendererCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''Component] ''DebugRenderer

instance NodeComponent DebugRenderer where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = DebugRenderer::GetTypeStatic();
    return &h;
  } |]

instance Createable (Ptr DebugRenderer) where 
  type CreationOptions (Ptr DebugRenderer) = Ptr Context

  newObject ptr = liftIO [C.exp| DebugRenderer* { new DebugRenderer($(Context* ptr)) } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(DebugRenderer* ptr) } |]

-- | Set the camera viewpoint. Call before rendering, or before adding geometry if you want to use culling.
debugRendererSetView :: (Parent DebugRenderer a, Pointer p a, MonadIO m, Parent Camera b, Pointer pcam b)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> pcam -- ^ Pointer to camera or ascentor
  -> m ()
debugRendererSetView p pcam = liftIO $ do 
  let ptr = parentPointer p
      ptrcam = parentPointer pcam
  [C.exp| void { $(DebugRenderer* ptr)->SetView($(Camera* ptrcam)) } |]

class DebugRendererAddLine a where 
  -- | Add line with color from struct or unsigned value
  debugRendererAddLine :: (Parent DebugRenderer b, Pointer p b, MonadIO m)
    => p -- ^ Pointer to DebugRenderer or ascentor
    -> Vector3 -- ^ Start
    -> Vector3 -- ^ End
    -> a -- ^ color value
    -> Bool -- ^ depthTest
    -> m ()

-- | Add a line.
instance DebugRendererAddLine Color where 
  debugRendererAddLine p s e c d = liftIO $ 
    with s $ \s' -> with e $ \e' -> with c $ \c' -> do 
      let ptr = parentPointer p
          d' = fromBool d
      [C.exp| void { $(DebugRenderer* ptr)->AddLine(*$(Vector3* s'), *$(Vector3* e'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a line with color already converted to unsigned.
instance DebugRendererAddLine Word where 
  debugRendererAddLine p s e c d = liftIO $ 
    with s $ \s' -> with e $ \e' -> do 
      let ptr = parentPointer p
          d' = fromBool d
          c' = fromIntegral c
      [C.exp| void { $(DebugRenderer* ptr)->AddLine(*$(Vector3* s'), *$(Vector3* e'), $(unsigned int c'), $(int d') != 0) } |]

class DebugRendererAddTriangle a where
  -- | Add a triangle with color from struct or unsigned value
  debugRendererAddTriangle :: (Parent DebugRenderer b, Pointer p b, MonadIO m)
    => p -- ^ Pointer to DebugRenderer or ascentor
    -> Vector3 -- ^ v1
    -> Vector3 -- ^ v2
    -> Vector3 -- ^ v3 
    -> a -- ^ color
    -> Bool -- ^ depthTest
    -> m ()

-- | Add a triangle.
instance DebugRendererAddTriangle Color where 
  debugRendererAddTriangle p v1 v2 v3 c d = liftIO $ 
    with v1 $ \v1' -> with v2 $ \v2' -> with v3 $ \v3' -> with c $ \c' -> do 
      let ptr = parentPointer p
          d' = fromBool d
      [C.exp| void { $(DebugRenderer* ptr)->AddTriangle(*$(Vector3* v1'), *$(Vector3* v2'), *$(Vector3* v3'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a triangle with color already converted to unsigned.
instance DebugRendererAddTriangle Word where 
  debugRendererAddTriangle p v1 v2 v3 c d = liftIO $ 
    with v1 $ \v1' -> with v2 $ \v2' -> with v3 $ \v3' -> do 
      let ptr = parentPointer p
          d' = fromBool d
          c' = fromIntegral c
      [C.exp| void { $(DebugRenderer* ptr)->AddTriangle(*$(Vector3* v1'), *$(Vector3* v2'), *$(Vector3* v3'), $(unsigned int c'), $(int d') != 0) } |]

-- | Add a scene node represented as its coordinate axes.
debugRendererAddNode :: (Parent DebugRenderer a, Pointer p a, MonadIO m, Parent Node b, Pointer pnode b)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> pnode -- ^ Pointer to node or ascentor
  -> Float -- ^ scale
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddNode p pnode scale d = liftIO $ do 
  let ptr = parentPointer p
      ptrnode = parentPointer pnode 
      scale' = realToFrac scale
      d' = fromBool d
  [C.exp| void { $(DebugRenderer* ptr)->AddNode($(Node* ptrnode), $(float scale'), $(int d') != 0) } |]

-- | Add a bounding box.
debugRendererAddBoundingBox :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> BoundingBox -- ^ box
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddBoundingBox p box c d = liftIO $ with box $ \box' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d 
  [C.exp| void { $(DebugRenderer* ptr)->AddBoundingBox(*$(BoundingBox* box'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a bounding box with transform.
debugRendererAddBoundingBoxWithTransform :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> BoundingBox -- ^ box
  -> Matrix3x4 -- ^ transform
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddBoundingBoxWithTransform p box t c d = liftIO $ with box $ \box' -> with t $ \t' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d 
  [C.exp| void { $(DebugRenderer* ptr)->AddBoundingBox(*$(BoundingBox* box'), *$(Matrix3x4* t'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a frustum.
debugRendererAddFrustum :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> Frustum -- ^ frustum
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddFrustum p f c d = liftIO $ with f $ \f' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d 
  [C.exp| void { $(DebugRenderer* ptr)->AddFrustum(*$(Frustum* f'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a polyhedron.
debugRendererAddPolyhedron :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> Polyhedron -- ^ poly
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddPolyhedron p ph c d = liftIO $ with ph $ \ph' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d 
  [C.exp| void { $(DebugRenderer* ptr)->AddPolyhedron(*$(Polyhedron* ph'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a sphere.
debugRendererAddSphere :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> Sphere -- ^ sphere
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddSphere p s c d = liftIO $ with s $ \s' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d 
  [C.exp| void { $(DebugRenderer* ptr)->AddSphere(*$(Sphere* s'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a cylinder
debugRendererAddCylinder :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> Vector3 -- ^ position
  -> Float -- ^ radius
  -> Float -- ^ height
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddCylinder p pv r h c d = liftIO $ with pv $ \pv' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d 
      r' = realToFrac r      
      h' = realToFrac h
  [C.exp| void { $(DebugRenderer* ptr)->AddCylinder(*$(Vector3* pv'), $(float r'), $(float h'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a skeleton.
debugRendererAddSkeleton :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> Ptr Skeleton -- ^ Skeleton
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddSkeleton p psk c d = liftIO $ with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d 
  [C.exp| void { $(DebugRenderer* ptr)->AddSkeleton(*$(Skeleton* psk), *$(Color* c'), $(int d') != 0) } |]

-- | Add a triangle mesh.
debugRendererAddTriangleMesh :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> Ptr () -- ^ vertex data 
  -> Word -- ^ vertex size
  -> Ptr () -- ^ index data 
  -> Word -- ^ index size
  -> Word -- ^ index start
  -> Word -- ^ index count
  -> Matrix3x4 -- ^ transform
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddTriangleMesh p vdata vsize idata isize istart icount tr c d = liftIO $ with tr $ \tr' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d 
      vsize' = fromIntegral vsize
      isize' = fromIntegral isize
      istart' = fromIntegral istart
      icount' = fromIntegral icount
  [C.exp| void { $(DebugRenderer* ptr)->AddTriangleMesh($(void* vdata), $(unsigned int vsize'), $(void* idata), $(unsigned int isize'), $(unsigned int istart'), $(unsigned int icount'), *$(Matrix3x4* tr'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a circle.
debugRendererAddCircle :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> Vector3 -- ^ center 
  -> Vector3 -- ^ normal 
  -> Float -- ^ radius
  -> Color -- ^ color
  -> Int -- ^ steps (def 64)
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddCircle p cv nv r c stps d = liftIO $ with cv $ \cv' -> with nv $ \nv' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d 
      stps' = fromIntegral stps
      r' = realToFrac r
  [C.exp| void { $(DebugRenderer* ptr)->AddCircle(*$(Vector3* cv'), *$(Vector3* nv'), $(float r'), *$(Color* c'), $(int stps'), $(int d') != 0) } |]

-- | Add a cross.
debugRendererAddCross :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> Vector3 -- ^ center 
  -> Float -- ^ size
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddCross p cv s c d = liftIO $ with cv $ \cv' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d
      s' = realToFrac s
  [C.exp| void { $(DebugRenderer* ptr)->AddCross(*$(Vector3* cv'), $(float s'), *$(Color* c'), $(int d') != 0) } |]

-- | Add a quad on the XZ plane.
debugRendererAddQuad :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> Vector3 -- ^ center 
  -> Float -- ^ width
  -> Float -- ^ height
  -> Color -- ^ color
  -> Bool -- ^ depth test
  -> m ()
debugRendererAddQuad p cv w h c d = liftIO $ with cv $ \cv' -> with c $ \c' -> do 
  let ptr = parentPointer p
      d' = fromBool d
      w' = realToFrac w
      h' = realToFrac h
  [C.exp| void { $(DebugRenderer* ptr)->AddQuad(*$(Vector3* cv'), $(float w'), $(float h'), *$(Color* c'), $(int d') != 0) } |]

-- | Update vertex buffer and render all debug lines. The viewport and rendertarget should be set before.
debugRendererRender :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> m ()
debugRendererRender p = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| void { $(DebugRenderer* ptr)->Render() } |]

-- | Return the view transform.
debugRendererGetView :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> m Matrix3x4
debugRendererGetView p = liftIO $ do 
  let ptr = parentPointer p
  peek =<< [C.exp| const Matrix3x4* { &$(DebugRenderer* ptr)->GetView() } |]

-- | Return the projection transform.
debugRendererGetProjection :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> m Matrix4
debugRendererGetProjection p = liftIO $ do 
  let ptr = parentPointer p
  peek =<< [C.exp| const Matrix4* { &$(DebugRenderer* ptr)->GetProjection() } |]

-- | Return the view frustum.
debugRendererGetFrustum :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> m Frustum
debugRendererGetFrustum p = liftIO $ do 
  let ptr = parentPointer p
  peek =<< [C.exp| const Frustum* { &$(DebugRenderer* ptr)->GetFrustum() } |]

-- | Check whether a bounding box is inside the view frustum.
debugRendererIsInside :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> BoundingBox -- ^ box
  -> m Bool
debugRendererIsInside p bb = liftIO $ with bb $ \bb' -> do 
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int) $(DebugRenderer* ptr)->IsInside(*$(BoundingBox* bb')) } |]

-- | Return whether has something to render.
debugRendererHasContent :: (Parent DebugRenderer a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to DebugRenderer or ascentor
  -> m Bool
debugRendererHasContent p = liftIO $ do 
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int) $(DebugRenderer* ptr)->HasContent() } |]
