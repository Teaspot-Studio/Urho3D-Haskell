{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.Material(
    Material
  , SharedMaterial
  , materialContext
  , materialSetFillMode
  , materialSetNumTechniques
  , materialSetPixelShaderDefines
  , materialSetShaderParameter
  , materialSetTechnique
  , materialSetTexture
  , materialSetVertexShaderDefines
  , materialClone
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Material
import Data.Monoid
import Foreign
import Foreign.C

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Graphics.Technique
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx
  <> materialCntx
  <> resourceContext
  <> objectContext
  <> sharedMaterialPtrCntx
  <> contextContext
  <> textureContext
  <> variantContext
  <> techniqueContext
  )

C.include "<Urho3D/Graphics/Material.h>"
C.using "namespace Urho3D"

materialContext :: C.Context
materialContext = materialCntx
  <> resourceContext
  <> sharedMaterialPtrCntx

deriveParents [''Object, ''Resource] ''Material

instance ResourceType Material where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Material::GetTypeStatic().Value() } |]

instance Creatable (Ptr Material) where
  type CreationOptions (Ptr Material) = Ptr Context
  newObject ptr = liftIO [C.exp| Material* {new Material($(Context* ptr))} |]
  deleteObject ptr = liftIO [C.exp| void {delete $(Material* ptr)} |]

sharedPtr "Material"

-- | Set number of techniques.
materialSetNumTechniques :: (Parent Material a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to material or acenstor
  -> Word -- ^ number
  -> m ()
materialSetNumTechniques p num = liftIO $ do
  let ptr = parentPointer p
      num' = fromIntegral num
  [C.exp| void { $(Material* ptr)->SetNumTechniques($(unsigned int num')) } |]

-- | Set number of techniques.
materialSetTechnique :: (Parent Material a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to material or acenstor
  -> Word -- ^ index
  -> Ptr Technique -- ^ tech
  -> Word -- ^ quality level (default 0)
  -> Float -- ^ lod distance (default 0.0)
  -> m ()
materialSetTechnique p index tech qualityLevel lodDistance = liftIO $ do
  let ptr = parentPointer p
      index' = fromIntegral index
      qualityLevel' = fromIntegral qualityLevel
      lodDistance' = realToFrac lodDistance
  [C.exp| void { $(Material* ptr)->SetTechnique($(unsigned int index'), $(Technique* tech), $(unsigned int qualityLevel'), $(float lodDistance')) } |]

-- | Set additional vertex shader defines. Separate multiple defines with spaces. Setting defines at the material level causes technique(s) to be cloned as necessary.
materialSetVertexShaderDefines :: (Parent Material a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to material or acenstor
  -> String -- ^ Defines separated by space
  -> m ()
materialSetVertexShaderDefines p defines = liftIO $ withCString defines $ \defines' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Material* ptr)->SetVertexShaderDefines(String($(const char* defines'))) } |]

-- | Set additional pixel shader defines. Separate multiple defines with spaces. Setting defines at the material level causes technique(s) to be cloned as necessary.
materialSetPixelShaderDefines :: (Parent Material a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to material or acenstor
  -> String -- ^ Defines separated by space
  -> m ()
materialSetPixelShaderDefines p defines = liftIO $ withCString defines $ \defines' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Material* ptr)->SetPixelShaderDefines(String($(const char* defines'))) } |]

-- | Set shader parameter.
materialSetShaderParameter :: (Parent Material a, Pointer p a, VariantStorable b, MonadIO m)
  => p -- ^ Pointer to material or acenstor
  -> String -- ^ Name of parameter
  -> b -- ^ Value for shader
  -> m ()
materialSetShaderParameter p name v = liftIO $ withCString name $ \name' -> withVariant v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Material* ptr)->SetShaderParameter(String($(const char* name')), *$(Variant* v')) } |]

-- | Set material texture unit
materialSetTexture :: (Parent Material a, Pointer p a, Parent Texture b, Pointer ptex b, MonadIO m)
  => p -- ^ Pointer to material or acenstor
  -> TextureUnit -- ^ Slot for texture
  -> ptex -- ^ Pointer to texture
  -> m ()
materialSetTexture p unit texture = liftIO $ do
  let ptr = parentPointer p
      unit' = fromIntegral . fromEnum $ unit
      texture' = parentPointer texture
  [C.exp| void { $(Material* ptr)->SetTexture((TextureUnit)$(int unit'), $(Texture* texture')) } |]

-- | Set polygon fill mode. Interacts with the camera's fill mode setting so that the "least filled" mode will be used.
materialSetFillMode :: (Parent Material a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to material or acenstor
  -> FillMode
  -> m ()
materialSetFillMode p mode = liftIO $ do
  let ptr = parentPointer p
      mode' = fromIntegral . fromEnum $ mode
  [C.exp| void { $(Material* ptr)->SetFillMode((FillMode)$(int mode')) } |]

-- | Clone the material.
materialClone :: (Parent Material a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to material or acenstor
  -> String -- ^ Clone name (default "")
  -> m (SharedPtr Material)
materialClone p name = liftIO $ withCString name $ \name' -> do
  let ptr = parentPointer p
  peekSharedPtr =<< [C.exp| SharedMaterial* { new SharedPtr<Material>($(Material* ptr)->Clone(String($(const char* name')))) } |]
