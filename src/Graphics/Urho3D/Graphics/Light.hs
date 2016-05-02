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
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Light
import Graphics.Urho3D.Monad
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO) 
import Foreign 

import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Scene.Node

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Graphics.Internal.Drawable 
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Parent
import Text.RawString.QQ

C.context (C.cppCtx <> lightCntx <> componentContext <> stringHashContext <> drawableCntx <> animatableContext <> serializableContext <> objectContext)
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
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = Light::GetTypeStatic();
    return &h;
  } |]

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
