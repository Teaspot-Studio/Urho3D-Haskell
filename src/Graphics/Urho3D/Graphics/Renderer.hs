{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Renderer(
    Renderer
  , rendererContext
  , rendererGetTextureQuality
  , rendererSetTextureQuality
  , rendererGetMaterialQuality
  , rendererSetMaterialQuality
  , rendererGetSpecularLighting
  , rendererSetSpecularLighting
  , rendererGetDrawShadows
  , rendererSetDrawShadows
  , rendererGetShadowMapSize
  , rendererSetShadowMapSize
  , rendererGetShadowQuality
  , rendererSetShadowQuality
  , rendererGetMaxOccluderTriangles
  , rendererSetMaxOccluderTriangles
  , rendererGetDynamicInstancing
  , rendererSetDynamicInstancing
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Renderer
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.C.Types

C.context (C.cppCtx <> rendererCntx <> objectContext)
C.include "<Urho3D/Graphics/Renderer.h>"
C.using "namespace Urho3D"

rendererContext :: C.Context 
rendererContext = objectContext <> rendererCntx

instance Parent Object Renderer where 
  castToParent ptr = [C.pure| Object* { (Object*)$(Renderer* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Renderer* { (Renderer*)$(Object* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Subsystem Renderer where 
  getSubsystemImpl ptr = [C.exp| Renderer* { $(Object* ptr)->GetSubsystem<Renderer>() } |]

-- | Return texture quality level.
rendererGetTextureQuality :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> m Quality
rendererGetTextureQuality p = liftIO $ do 
  let ptr = parentPointer p 
  toEnum . fromIntegral <$> [C.exp| int { $(Renderer* ptr)->GetTextureQuality() } |]

-- | Sets texture quality level
rendererSetTextureQuality :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> Quality -- ^ Quality level
  -> m ()
rendererSetTextureQuality p q = liftIO $ do 
  let ptr = parentPointer p 
      e = fromIntegral $ fromEnum q 
  [C.exp| void {$(Renderer* ptr)->SetTextureQuality($(int e))} |]

-- | Return material quality level.
rendererGetMaterialQuality :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> m Quality
rendererGetMaterialQuality p = liftIO $ do 
  let ptr = parentPointer p 
  toEnum . fromIntegral <$> [C.exp| int { $(Renderer* ptr)->GetMaterialQuality() } |]

-- | Sets texture quality level
rendererSetMaterialQuality :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> Quality -- ^ Quality level
  -> m ()
rendererSetMaterialQuality p q = liftIO $ do 
  let ptr = parentPointer p 
      e = fromIntegral $ fromEnum q 
  [C.exp| void {$(Renderer* ptr)->SetMaterialQuality($(int e))} |]

-- | Is specular lighting on?
rendererGetSpecularLighting :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> m Bool
rendererGetSpecularLighting p = liftIO $ do 
  let ptr = parentPointer p 
  (/= 0) <$> [C.exp| int {$(Renderer* ptr)->GetSpecularLighting()}|]

-- | Switches on/off specular lighting
rendererSetSpecularLighting :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> Bool -- ^ Flag
  -> m ()
rendererSetSpecularLighting p flag = liftIO $ do 
  let ptr = parentPointer p 
      flag' = if flag then 1 else 0
  [C.exp| void {$(Renderer* ptr)->SetSpecularLighting($(int flag') != 0)}|]

-- | Is shadows on?
rendererGetDrawShadows :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> m Bool
rendererGetDrawShadows p = liftIO $ do 
  let ptr = parentPointer p 
  (/= 0) <$> [C.exp| int {$(Renderer* ptr)->GetDrawShadows()}|]

-- | Switches on/off shadows
rendererSetDrawShadows :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> Bool -- ^ Flag
  -> m ()
rendererSetDrawShadows p flag = liftIO $ do 
  let ptr = parentPointer p 
      flag' = if flag then 1 else 0
  [C.exp| void {$(Renderer* ptr)->SetDrawShadows($(int flag') != 0)}|]

-- | Returns length of side of shadow texture
rendererGetShadowMapSize :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> m Int 
rendererGetShadowMapSize p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int {$(Renderer* ptr)->GetShadowMapSize()}|]

-- | Sets length of side of shadow texture
rendererSetShadowMapSize :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> Int -- ^ Size, usually power of 2
  -> m ()
rendererSetShadowMapSize p s = liftIO $ do 
  let ptr = parentPointer p 
      s' = fromIntegral s 
  [C.exp| void {$(Renderer* ptr)->SetShadowMapSize($(int s'))} |]

-- | Return shadow quality level.
rendererGetShadowQuality :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> m ShadowQuality
rendererGetShadowQuality p = liftIO $ do 
  let ptr = parentPointer p 
  toEnum . fromIntegral <$> [C.exp| int { $(Renderer* ptr)->GetShadowQuality() } |]

-- | Sets shadow quality level
rendererSetShadowQuality :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> ShadowQuality -- ^ Quality level
  -> m ()
rendererSetShadowQuality p q = liftIO $ do 
  let ptr = parentPointer p 
      e = fromIntegral $ fromEnum q 
  [C.exp| void {$(Renderer* ptr)->SetShadowQuality((ShadowQuality) $(int e))} |]

-- | Returns maximum number of triangles that occluder lefts on scene
rendererGetMaxOccluderTriangles :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> m Int 
rendererGetMaxOccluderTriangles p = liftIO $ do 
  let ptr = parentPointer p 
  fromIntegral <$> [C.exp| int {$(Renderer* ptr)->GetMaxOccluderTriangles()}|]

-- | Sets maximum number of triangles that occluder lefts on scene
rendererSetMaxOccluderTriangles :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> Int -- ^ Size, usually power of 2
  -> m ()
rendererSetMaxOccluderTriangles p s = liftIO $ do 
  let ptr = parentPointer p 
      s' = fromIntegral s 
  [C.exp| void {$(Renderer* ptr)->SetMaxOccluderTriangles($(int s'))} |]

-- | Is dynamic instancing on?
rendererGetDynamicInstancing :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> m Bool
rendererGetDynamicInstancing p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int {$(Renderer* ptr)->GetDynamicInstancing()}|]

-- | Switches on/off dynamic instancing
rendererSetDynamicInstancing :: (Parent Renderer a, Pointer p a, MonadIO m) => p -- ^ Pointer to renderer or child
  -> Bool -- ^ Flag
  -> m ()
rendererSetDynamicInstancing p flag = liftIO $ do 
  let ptr = parentPointer p 
      flag' = fromBool flag
  [C.exp| void {$(Renderer* ptr)->SetDynamicInstancing($(int flag') != 0)}|]