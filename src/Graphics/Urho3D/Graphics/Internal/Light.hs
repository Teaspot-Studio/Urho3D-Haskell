module Graphics.Urho3D.Graphics.Internal.Light(
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
  , lightCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Control.DeepSeq 
import Control.Lens 
import GHC.Generics 
import qualified Data.Map as Map

data Light 
data PODVectorLightPtr

-- | Shadow depth bias parameters.
data BiasParameters = BiasParameters {
  _biasParametersBias :: {-# UNPACK #-} !Float -- ^ Constant bias.
, _biasParametersSlopeScaleBias :: {-# UNPACK #-} !Float -- ^ Slope scaled bias.
} deriving (Eq, Show, Generic)

makeFields ''BiasParameters
instance NFData BiasParameters

-- | Cascaded shadow map parameters.
data CascadeParameters = CascadeParameters {
  _cascadeParametersSplit1 :: {-# UNPACK #-} !Float -- ^ Far clip values of the splits.
, _cascadeParametersSplit2 :: {-# UNPACK #-} !Float -- ^ Far clip values of the splits.
, _cascadeParametersSplit3 :: {-# UNPACK #-} !Float -- ^ Far clip values of the splits.
, _cascadeParametersSplit4 :: {-# UNPACK #-} !Float -- ^ Far clip values of the splits.
, _cascadeParametersFadeStart :: {-# UNPACK #-} !Float -- ^ The point relative to the total shadow range where shadow fade begins (0.0 - 1.0)
, _cascadeParametersBiasAutoAdjust :: {-# UNPACK #-} !Float -- ^ Automatic depth bias adjustment strength.
} deriving (Eq, Show, Generic)

makeFields ''CascadeParameters
instance NFData CascadeParameters

-- | Shadow map focusing parameters.
data FocusParameters = FocusParameters {
  _focusParametersFocus :: !Bool -- ^ Focus flag.
, _focusParametersNonUniform :: !Bool -- ^ Non-uniform focusing flag.
, _focusParametersAutoSize :: !Bool -- ^ Auto-size (reduce resolution when far away) flag.
, _focusParametersQuantize :: {-# UNPACK #-} !Float -- ^ Focus quantization.
, _focusParametersMinView :: {-# UNPACK #-} !Float -- ^ Minimum view size.
} deriving (Eq, Show, Generic)

makeFields ''FocusParameters
instance NFData FocusParameters

lightCntx :: C.Context 
lightCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Light", [t| Light |])
    , (C.TypeName "BiasParameters", [t| BiasParameters |])
    , (C.TypeName "CascadeParameters", [t| CascadeParameters |])
    , (C.TypeName "FocusParameters", [t| FocusParameters |])
    , (C.TypeName "PODVectorLightPtr", [t| PODVectorLightPtr |])
    ]
  } 