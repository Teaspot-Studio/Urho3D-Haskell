{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Light(
    Light
  , PODVectorLightPtr
  , BiasParameters(..)
  , HasBias(..)
  , HasSlopeScaleBias(..)
  , CascadeParameters(..)
  , HasSplit1(..)
  , HasSplit2(..)
  , HasSplit3(..)
  , HasSplit4(..)
  , HasFadeStart(..)
  , HasBiasAutoAdjust(..)
  , FocusParameters(..)
  , HasFocus(..)
  , HasNonUniform(..)
  , HasAutoSize(..)
  , HasQuantize(..)
  , HasMinView(..)
  , LightType(..)
  , lightContext
  , lightSetLightType
  , lightSetRange
  , lightSetPerVertex
  , lightSetColor
  , lightSetSpecularIntensity
  , lightSetBrightness
  , lightSetFov
  , lightSetAspectRatio
  , lightSetFadeDistance
  , lightSetShadowFadeDistance
  , lightSetShadowBias
  , lightSetShadowCascade
  , lightSetShadowFocus
  , lightSetShadowIntensity
  , lightSetShadowResolution
  , lightSetShadowNearFarRatio
  , lightSetRampTexture
  , lightSetShapeTexture
  , lightGetLightType
  , lightGetPerVertex
  , lightGetColor
  , lightGetSpecularIntensity
  , lightGetBrightness
  , lightGetEffectiveColor
  , lightGetEffectiveSpecularIntensity
  , lightGetRange
  , lightGetFov
  , lightGetAspectRatio
  , lightGetFadeDistance
  , lightGetShadowFadeDistance
  , lightGetShadowBias
  , lightGetShadowCascade
  , lightGetShadowFocus
  , lightGetShadowIntensity
  , lightGetShadowResolution
  , lightGetShadowNearFarRatio
  , lightGetRampTexture
  , lightGetShapeTexture
  , lightGetFrustum
  , lightGetNumShadowSplits
  , lightIsNegative
  , LightSetIntensitySortValue(..)
  , lightSetLightQueue
  , lightGetVolumeTransform
  , lightGetLightQueue
  , lightGetIntensityDivisor
  , lightSetRampTextureAttr
  , lightSetShapeTextureAttr
  , lightGetRampTextureAttr
  , lightGetShapeTextureAttr
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Light
import Graphics.Urho3D.Monad
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import Foreign
import Text.RawString.QQ

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Scene.Node

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Graphics.Internal.Drawable
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Graphics.Batch
import Graphics.Urho3D.Graphics.Camera
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.Frustum
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Math.Matrix3x4

C.context (C.cppCtx
  <> lightCntx
  <> componentContext
  <> stringHashContext
  <> drawableCntx
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> colorContext
  <> frustumContext
  <> textureContext
  <> boundingBoxContext
  <> batchContext
  <> matrix3x4Context
  <> variantContext
  <> cameraContext)

C.include "<Urho3D/Graphics/Light.h>"
C.using "namespace Urho3D"

C.verbatim [r|
template <class T>
class Traits
{
public:
    struct AlignmentFinder
    {
      char a;
      T b;
    };

    enum {AlignmentOf = sizeof(AlignmentFinder) - sizeof(T)};
};
|]

podVectorPtr "Light"

lightContext :: C.Context
lightContext = lightCntx <> componentContext <> stringHashContext

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Drawable] ''Light

instance NodeComponent Light where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Light::GetTypeStatic().Value() } |]

-- | Light types
data LightType =
    LT'Directional
  | LT'Spot
  | LT'Point
  deriving (Eq, Show, Read, Enum, Bounded)

instance Storable BiasParameters where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(BiasParameters) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<BiasParameters>::AlignmentOf } |]
  peek ptr = do
    _biasParametersBias <- realToFrac <$> [C.exp| float {$(BiasParameters* ptr)->constantBias_} |]
    _biasParametersSlopeScaleBias <- realToFrac <$> [C.exp| float {$(BiasParameters* ptr)->slopeScaledBias_} |]
    return BiasParameters {..}
  poke ptr (BiasParameters {..}) = [C.block| void {
      $(BiasParameters* ptr)->constantBias_ = $(float _biasParametersBias');
      $(BiasParameters* ptr)->slopeScaledBias_ = $(float _biasParametersSlopeScaleBias');
    } |]
    where
    _biasParametersBias' = realToFrac _biasParametersBias
    _biasParametersSlopeScaleBias' = realToFrac _biasParametersSlopeScaleBias

instance Storable CascadeParameters where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(CascadeParameters) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<CascadeParameters>::AlignmentOf } |]
  peek ptr = do
    _cascadeParametersSplit1 <- realToFrac <$> [C.exp| float { $(CascadeParameters* ptr)->splits_[0] } |]
    _cascadeParametersSplit2 <- realToFrac <$> [C.exp| float { $(CascadeParameters* ptr)->splits_[1] } |]
    _cascadeParametersSplit3 <- realToFrac <$> [C.exp| float { $(CascadeParameters* ptr)->splits_[2] } |]
    _cascadeParametersSplit4 <- realToFrac <$> [C.exp| float { $(CascadeParameters* ptr)->splits_[3] } |]
    _cascadeParametersFadeStart <- realToFrac <$> [C.exp| float { $(CascadeParameters* ptr)->fadeStart_ } |]
    _cascadeParametersBiasAutoAdjust <- realToFrac <$> [C.exp| float { $(CascadeParameters* ptr)->biasAutoAdjust_ } |]
    return CascadeParameters {..}
  poke ptr (CascadeParameters {..}) = [C.block| void {
      $(CascadeParameters* ptr)->splits_[0] = $(float _cascadeParametersSplit1');
      $(CascadeParameters* ptr)->splits_[1] = $(float _cascadeParametersSplit2');
      $(CascadeParameters* ptr)->splits_[2] = $(float _cascadeParametersSplit3');
      $(CascadeParameters* ptr)->splits_[3] = $(float _cascadeParametersSplit4');
      $(CascadeParameters* ptr)->fadeStart_ = $(float _cascadeParametersFadeStart');
      $(CascadeParameters* ptr)->biasAutoAdjust_ = $(float _cascadeParametersBiasAutoAdjust');
    } |]
    where
    _cascadeParametersSplit1' = realToFrac _cascadeParametersSplit1
    _cascadeParametersSplit2' = realToFrac _cascadeParametersSplit2
    _cascadeParametersSplit3' = realToFrac _cascadeParametersSplit3
    _cascadeParametersSplit4' = realToFrac _cascadeParametersSplit4
    _cascadeParametersFadeStart' = realToFrac _cascadeParametersFadeStart
    _cascadeParametersBiasAutoAdjust' = realToFrac _cascadeParametersBiasAutoAdjust

instance Storable FocusParameters where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(FocusParameters) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<FocusParameters>::AlignmentOf } |]
  peek ptr = do
    _focusParametersFocus <- toBool <$> [C.exp| int { (int)$(FocusParameters* ptr)->focus_ } |]
    _focusParametersNonUniform <- toBool <$> [C.exp| int { (int)$(FocusParameters* ptr)->nonUniform_ } |]
    _focusParametersAutoSize <- toBool <$> [C.exp| int { (int)$(FocusParameters* ptr)->autoSize_ } |]
    _focusParametersQuantize <- realToFrac <$> [C.exp| float { $(FocusParameters* ptr)->quantize_ } |]
    _focusParametersMinView <- realToFrac <$> [C.exp| float { $(FocusParameters* ptr)->minView_ } |]
    return FocusParameters {..}
  poke ptr (FocusParameters {..}) = [C.block| void {
      $(FocusParameters* ptr)->focus_ = $(int _focusParametersFocus') != 0;
      $(FocusParameters* ptr)->nonUniform_ = $(int _focusParametersNonUniform') != 0;
      $(FocusParameters* ptr)->autoSize_ = $(int _focusParametersAutoSize') != 0;
      $(FocusParameters* ptr)->quantize_ = $(float _focusParametersQuantize');
      $(FocusParameters* ptr)->minView_ = $(float _focusParametersMinView');
    } |]
    where
    _focusParametersFocus' = fromBool _focusParametersFocus
    _focusParametersNonUniform' = fromBool _focusParametersNonUniform
    _focusParametersAutoSize' = fromBool _focusParametersAutoSize
    _focusParametersQuantize' = realToFrac _focusParametersQuantize
    _focusParametersMinView' = realToFrac _focusParametersMinView

-- | Set light type
lightSetLightType :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to light
  -> LightType -- ^ Type of light
  -> m ()
lightSetLightType p lt = liftIO $ do
  let ptr = parentPointer p
      i = fromIntegral $ fromEnum lt
  [C.exp| void { $(Light* ptr)->SetLightType((LightType)$(int i)) } |]

-- | Set range.
lightSetRange :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to light
  -> Float -- ^ Light max range
  -> m ()
lightSetRange p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(Light* ptr)->SetRange($(float v')) } |]

-- | Set vertex lighting mode.
lightSetPerVertex :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Bool -- ^ enable
  -> m ()
lightSetPerVertex p e = liftIO $ do
  let ptr = parentPointer p
      e' = fromBool e
  [C.exp| void {$(Light* ptr)->SetPerVertex($(int e') != 0)} |]

-- | Set color.
lightSetColor :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Color -- ^ color
  -> m ()
lightSetColor p c = liftIO $ with c $ \c' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Light* ptr)->SetColor(*$(Color* c'))} |]

-- | Set specular intensity. Zero disables specular calculations.
lightSetSpecularIntensity :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Float -- ^ intensity
  -> m ()
lightSetSpecularIntensity p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void {$(Light* ptr)->SetSpecularIntensity($(float d'))} |]

-- | Set light brightness multiplier. Both the color and specular intensity are multiplied with this to get final values for rendering.
lightSetBrightness :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Float -- ^ brightness
  -> m ()
lightSetBrightness p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void {$(Light* ptr)->SetBrightness($(float d'))} |]

-- | Set spotlight field of view.
lightSetFov :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Float -- ^ fov
  -> m ()
lightSetFov p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void {$(Light* ptr)->SetFov($(float d'))} |]

-- | Set spotlight aspect ratio.
lightSetAspectRatio :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Float -- ^ aspect ratio
  -> m ()
lightSetAspectRatio p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void {$(Light* ptr)->SetAspectRatio($(float d'))} |]

-- | Set fade out start distance.
lightSetFadeDistance :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Float -- ^ distance
  -> m ()
lightSetFadeDistance p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void {$(Light* ptr)->SetFadeDistance($(float d'))} |]

-- | Set shadow fade out start distance. Only has effect if shadow distance is also non-zero.
lightSetShadowFadeDistance :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Float -- ^ distance
  -> m ()
lightSetShadowFadeDistance p d = liftIO $ do
  let ptr = parentPointer p
      d' = realToFrac d
  [C.exp| void {$(Light* ptr)->SetShadowFadeDistance($(float d'))} |]

-- | Set shadow depth bias parameters.
lightSetShadowBias :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> BiasParameters -- ^ parameters
  -> m ()
lightSetShadowBias p bp = liftIO $ with bp $ \bp' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Light* ptr)->SetShadowBias(*$(BiasParameters* bp'))} |]

-- | Set directional light cascaded shadow parameters.
lightSetShadowCascade :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> CascadeParameters -- ^ parameters
  -> m ()
lightSetShadowCascade p cp = liftIO $ with cp $ \cp' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Light* ptr)->SetShadowCascade(*$(CascadeParameters* cp'))} |]

-- | Set shadow map focusing parameters.
lightSetShadowFocus :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> FocusParameters -- ^ parameters
  -> m ()
lightSetShadowFocus p fp = liftIO $ with fp $ \fp' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Light* ptr)->SetShadowFocus(*$(FocusParameters* fp'))} |]

-- | Set light intensity in shadow between 0.0 - 1.0. 0.0 (the default) gives fully dark shadows.
lightSetShadowIntensity :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Float -- ^ itensity
  -> m ()
lightSetShadowIntensity p i = liftIO $ do
  let ptr = parentPointer p
      i' = realToFrac i
  [C.exp| void {$(Light* ptr)->SetShadowIntensity($(float i'))} |]

-- | Set shadow resolution between 0.25 - 1.0. Determines the shadow map to use.
lightSetShadowResolution :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Float -- ^ resoulution
  -> m ()
lightSetShadowResolution p rs = liftIO $ do
  let ptr = parentPointer p
      rs' = realToFrac rs
  [C.exp| void {$(Light* ptr)->SetShadowResolution($(float rs'))} |]

-- | Set shadow camera near/far clip distance ratio.
lightSetShadowNearFarRatio :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Float -- ^ near far ration
  -> m ()
lightSetShadowNearFarRatio p rs = liftIO $ do
  let ptr = parentPointer p
      rs' = realToFrac rs
  [C.exp| void {$(Light* ptr)->SetShadowNearFarRatio($(float rs'))} |]

-- | Set range attenuation texture.
lightSetRampTexture :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Ptr Texture -- ^ texture
  -> m ()
lightSetRampTexture p ptex = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(Light* ptr)->SetRampTexture($(Texture* ptex))} |]

-- | Set spotlight attenuation texture.
lightSetShapeTexture :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Ptr Texture -- ^ texture
  -> m ()
lightSetShapeTexture p ptex = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(Light* ptr)->SetShapeTexture($(Texture* ptex))} |]

-- | Return light type.
lightGetLightType :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m LightType
lightGetLightType p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(Light* ptr)->GetLightType()} |]

-- | Return vertex lighting mode.
lightGetPerVertex :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Bool
lightGetPerVertex p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Light* ptr)->GetPerVertex()} |]

-- | Return color.
lightGetColor :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Color
lightGetColor p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Color* {&$(Light* ptr)->GetColor()} |]

-- | Return specular intensity.
lightGetSpecularIntensity :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetSpecularIntensity p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetSpecularIntensity()} |]

-- | Return brightness multiplier.
lightGetBrightness :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetBrightness p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetBrightness()} |]

-- | Return effective color, multiplied by brightness. Do not multiply the alpha so that can compare against the default black color to detect a light with no effect.
lightGetEffectiveColor :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Color
lightGetEffectiveColor p = liftIO $ alloca $ \resptr -> do
  let ptr = parentPointer p
  [C.exp| void {
    *($(Color* resptr)) = $(Light* ptr)->GetEffectiveColor()
  } |]
  peek resptr

-- | Return effective specular intensity, multiplied by absolute value of brightness.
lightGetEffectiveSpecularIntensity :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetEffectiveSpecularIntensity p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetEffectiveSpecularIntensity()} |]

-- | Return range.
lightGetRange :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetRange p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetRange()} |]

-- | Return spotlight field of view.
lightGetFov :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetFov p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetFov()} |]

-- | Return spotlight aspect ratio.
lightGetAspectRatio :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetAspectRatio p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetAspectRatio()} |]

-- | Return fade start distance.
lightGetFadeDistance :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetFadeDistance p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetFadeDistance()} |]

-- | Return shadow fade start distance.
lightGetShadowFadeDistance :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetShadowFadeDistance p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetShadowFadeDistance()} |]

-- | Return shadow depth bias parameters.
lightGetShadowBias :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m BiasParameters
lightGetShadowBias p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const BiasParameters* {&$(Light* ptr)->GetShadowBias()} |]

-- | Return directional light cascaded shadow parameters.
lightGetShadowCascade :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m CascadeParameters
lightGetShadowCascade p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const CascadeParameters* {&$(Light* ptr)->GetShadowCascade()} |]

-- | Return shadow map focus parameters.
lightGetShadowFocus :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m FocusParameters
lightGetShadowFocus p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const FocusParameters* {&$(Light* ptr)->GetShadowFocus()} |]

-- | Return light intensity in shadow.
lightGetShadowIntensity :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetShadowIntensity p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetShadowIntensity()} |]

-- | Return shadow camera near/far clip distance ratio.
lightGetShadowResolution :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetShadowResolution p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetShadowResolution()} |]

-- | Return shadow camera near/far clip distance ratio.
lightGetShadowNearFarRatio :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetShadowNearFarRatio p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetShadowNearFarRatio()} |]

-- | Return range attenuation texture.
lightGetRampTexture :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m (Ptr Texture)
lightGetRampTexture p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Texture* {$(Light* ptr)->GetRampTexture()} |]

-- | Return spotlight attenuation texture.
lightGetShapeTexture :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m (Ptr Texture)
lightGetShapeTexture p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Texture* {$(Light* ptr)->GetShapeTexture()} |]

-- | Return spotlight frustum.
lightGetFrustum :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Frustum
lightGetFrustum p = liftIO $ alloca $ \resptr -> do
  let ptr = parentPointer p
  [C.exp| void {
    *($(Frustum* resptr)) = $(Light* ptr)->GetFrustum()
    } |]
  peek resptr

-- | Return number of shadow map cascade splits for a directional light, considering also graphics API limitations.
lightGetNumShadowSplits :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Int
lightGetNumShadowSplits p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int {$(Light* ptr)->GetNumShadowSplits()} |]

-- | Return whether light has negative (darkening) color.
lightIsNegative :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Bool
lightIsNegative p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Light* ptr)->IsNegative()} |]

class LightSetIntensitySortValue a where
  lightSetIntensitySortValue :: (Parent Light b, Pointer p b, MonadIO m)
    => p -- ^ Pointer to Light or ascentor
    -> a -- ^ value
    -> m ()

-- | Set sort value based on intensity and view distance.
instance LightSetIntensitySortValue Float where
  lightSetIntensitySortValue p d = liftIO $ do
    let ptr = parentPointer p
        d' = realToFrac d
    [C.exp| void {$(Light* ptr)->SetIntensitySortValue($(float d'))} |]

-- | Set sort value based on overall intensity over a bounding box.
instance LightSetIntensitySortValue BoundingBox where
  lightSetIntensitySortValue p b = liftIO $ with b $ \b' -> do
    let ptr = parentPointer p
    [C.exp| void {$(Light* ptr)->SetIntensitySortValue(*$(BoundingBox* b'))} |]

-- | Set light queue used for this light. Called by View.
lightSetLightQueue :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Ptr LightBatchQueue
  -> m ()
lightSetLightQueue p pb = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(Light* ptr)->SetLightQueue($(LightBatchQueue* pb))} |]

-- | Return light volume model transform.
lightGetVolumeTransform :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> Ptr Camera -- ^ camera
  -> m Matrix3x4
lightGetVolumeTransform p pcam = liftIO $ alloca $ \resptr -> do
  let ptr = parentPointer p
  [C.exp| void {
    *($(Matrix3x4* resptr)) = $(Light* ptr)->GetVolumeTransform($(Camera* pcam))
    } |]
  peek resptr

-- | Return light queue. Called by View.
lightGetLightQueue :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m (Ptr LightBatchQueue)
lightGetLightQueue p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| LightBatchQueue* {$(Light* ptr)->GetLightQueue()} |]

-- | Return a divisor value based on intensity for calculating the sort value.
lightGetIntensityDivisor :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m Float
lightGetIntensityDivisor p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Light* ptr)->GetIntensityDivisor()} |]

-- | Set ramp texture attribute.
lightSetRampTextureAttr :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> ResourceRef -- ^ value
  -> m ()
lightSetRampTextureAttr p rf = liftIO $ with rf $ \rf' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Light* ptr)->SetRampTextureAttr(*$(ResourceRef* rf'))} |]

-- | Set shape texture attribute.
lightSetShapeTextureAttr :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> ResourceRef -- ^ value
  -> m ()
lightSetShapeTextureAttr p rf = liftIO $ with rf $ \rf' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Light* ptr)->SetShapeTextureAttr(*$(ResourceRef* rf'))} |]

-- | Return ramp texture attribute.
lightGetRampTextureAttr :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m ResourceRef
lightGetRampTextureAttr p = liftIO $ alloca $ \resptr -> do
  let ptr = parentPointer p
  [C.exp| void {
    *($(ResourceRef* resptr)) = $(Light* ptr)->GetRampTextureAttr()
    } |]
  peek resptr

-- | Return shape texture attribute.
lightGetShapeTextureAttr :: (Parent Light a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Light or ascentor
  -> m ResourceRef
lightGetShapeTextureAttr p = liftIO $ alloca $ \resptr -> do
  let ptr = parentPointer p
  [C.exp| void {
    *($(ResourceRef* resptr)) =  $(Light* ptr)->GetShapeTextureAttr()
    } |]
  peek resptr
