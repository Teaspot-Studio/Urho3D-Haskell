{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Defs(
    graphDefsContext
  , PrimitiveType(..)
  , GeometryType(..)
  , BlendMode(..)
  , CompareMode(..)
  , CullMode(..)
  , Fillmode(..)
  , StencilOp(..)
  , LockState(..)
  , LegacyVertexElement(..)
  , VertexElementType(..)
  , VertexElementSemantic(..)
  , VertexElement(..)
  , HasElementType(..)
  , HasSemantic(..)
  , HasIndex(..)
  , HasPerInstance(..)
  , HasOffset(..)
  , TextureFilterMode(..)
  , TextureAddressMode(..)
  , TextureCoordinate(..)
  , TextureUsage(..)
  , CubeMapFace(..)
  , CubeMapLayout(..)
  , RenderSurfaceUpdateMode(..)
  , ShaderType(..)
  , ShaderParameterGroup(..)
  , TextureUnit(..)
  , maxMatrialTextureUnits
  , maxTextureUnits
  , vspAmbientstartcolor
  , vspAmbientendcolor
  , vspBillboardrot
  , vspCamerapos
  , vspClipplane
  , vspNearclip
  , vspFarclip
  , vspDepthmode
  , vspDeltatime
  , vspElapsedtime
  , vspFrustumsize
  , vspGbufferoffsets
  , vspLightdir
  , vspLightpos
  , vspModel
  , vspView
  , vspViewInv
  , vspViewproj
  , vspUoffset
  , vspVoffset
  , vspZone
  , vspLightmatrices
  , vspSkinmatrices
  , vspVertexlights
  , pspAmbientcolor
  , pspCamerapos
  , pspDeltatime
  , pspDepthreconstruct
  , pspElapsedtime
  , pspFogcolor
  , pspFogparams
  , pspGbufferinvsize
  , pspLightcolor
  , pspLightdir
  , pspLightpos
  , pspMatdiffcolor
  , pspMatemissivecolor
  , pspMatenvmapcolor
  , pspMatspeccolor
  , pspNearclip
  , pspFarclip
  , pspShadowcubeadjust
  , pspShadowdepthfade
  , pspShadowintensity
  , pspShadowmapinvsize
  , pspShadowsplits
  , pspLightmatrices
  , pspVSMShadowParams
  , pspRoughness
  , pspMetallic
  , dotScale
  , FaceCameraMode(..)
  , Quality(..)
  , ShadowQuality(..)
  , Mask(..)
  , maxRenderTargets
  , maxVertexStreams
  , maxConstantRegisters
  , bitsPerComponent
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Text.RawString.QQ

import Graphics.Urho3D.Graphics.Internal.Defs

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector3
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Foreign

C.context (C.cppCtx <> stringHashContext <> vector3Context <> graphDefsCntx)
C.include "<Urho3D/Graphics/GraphicsDefs.h>"
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

graphDefsContext :: C.Context
graphDefsContext = graphDefsCntx

instance Storable VertexElement where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(VertexElement) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<VertexElement>::AlignmentOf } |]
  peek ptr = do
    _vertexElementElementType <- toEnum . fromIntegral <$> [C.exp| int {$(VertexElement* ptr)->type_} |]
    _vertexElementSemantic <- toEnum . fromIntegral <$> [C.exp| int {$(VertexElement* ptr)->semantic_} |]
    _vertexElementIndex <- fromIntegral <$> [C.exp| unsigned char {$(VertexElement* ptr)->index_} |]
    _vertexElementPerInstance <- toBool <$> [C.exp| int {(int)$(VertexElement* ptr)->perInstance_} |]
    _vertexElementOffset <- fromIntegral <$> [C.exp| unsigned int {$(VertexElement* ptr)->offset_} |]
    return VertexElement {..}
  poke ptr (VertexElement {..}) = [C.block| void {
      $(VertexElement* ptr)->type_ = (VertexElementType)$(int _vertexElementElementType');
      $(VertexElement* ptr)->semantic_ = (VertexElementSemantic)$(int _vertexElementSemantic');
      $(VertexElement* ptr)->index_ = $(unsigned char _vertexElementIndex');
      $(VertexElement* ptr)->perInstance_ = $(int _vertexElementPerInstance') != 0;
      $(VertexElement* ptr)->offset_ = $(unsigned int _vertexElementOffset');
    } |]
    where
    _vertexElementElementType' = fromIntegral . fromEnum $ _vertexElementElementType
    _vertexElementSemantic' = fromIntegral . fromEnum $ _vertexElementSemantic
    _vertexElementIndex' = fromIntegral _vertexElementIndex
    _vertexElementPerInstance' = fromBool _vertexElementPerInstance
    _vertexElementOffset' = fromIntegral _vertexElementOffset

-- Inbuilt shader parameters.
vspAmbientstartcolor :: Ptr StringHash
vspAmbientstartcolor = [C.pure| const StringHash* { &VSP_AMBIENTSTARTCOLOR } |]

vspAmbientendcolor :: Ptr StringHash
vspAmbientendcolor = [C.pure| const StringHash* { &VSP_AMBIENTENDCOLOR } |]

vspBillboardrot :: Ptr StringHash
vspBillboardrot = [C.pure| const StringHash* { &VSP_BILLBOARDROT } |]

vspCamerapos :: Ptr StringHash
vspCamerapos = [C.pure| const StringHash* { &VSP_CAMERAPOS } |]

vspClipplane :: Ptr StringHash
vspClipplane = [C.pure| const StringHash* { &VSP_CLIPPLANE } |]

vspNearclip :: Ptr StringHash
vspNearclip = [C.pure| const StringHash* { &VSP_NEARCLIP } |]

vspFarclip :: Ptr StringHash
vspFarclip = [C.pure| const StringHash* { &VSP_FARCLIP } |]

vspDepthmode :: Ptr StringHash
vspDepthmode = [C.pure| const StringHash* { &VSP_DEPTHMODE } |]

vspDeltatime :: Ptr StringHash
vspDeltatime = [C.pure| const StringHash* { &VSP_DELTATIME } |]

vspElapsedtime :: Ptr StringHash
vspElapsedtime = [C.pure| const StringHash* { &VSP_ELAPSEDTIME } |]

vspFrustumsize :: Ptr StringHash
vspFrustumsize = [C.pure| const StringHash* { &VSP_FRUSTUMSIZE } |]

vspGbufferoffsets :: Ptr StringHash
vspGbufferoffsets = [C.pure| const StringHash* { &VSP_GBUFFEROFFSETS } |]

vspLightdir :: Ptr StringHash
vspLightdir = [C.pure| const StringHash* { &VSP_LIGHTDIR } |]

vspLightpos :: Ptr StringHash
vspLightpos = [C.pure| const StringHash* { &VSP_LIGHTPOS } |]

vspModel :: Ptr StringHash
vspModel = [C.pure| const StringHash* { &VSP_MODEL } |]

vspView :: Ptr StringHash
vspView = [C.pure| const StringHash* { &VSP_VIEW } |]

vspViewInv :: Ptr StringHash
vspViewInv = [C.pure| const StringHash* { &VSP_VIEWINV } |]

vspViewproj :: Ptr StringHash
vspViewproj = [C.pure| const StringHash* { &VSP_VIEWPROJ } |]

vspUoffset :: Ptr StringHash
vspUoffset = [C.pure| const StringHash* { &VSP_UOFFSET } |]

vspVoffset :: Ptr StringHash
vspVoffset = [C.pure| const StringHash* { &VSP_VOFFSET } |]

vspZone :: Ptr StringHash
vspZone = [C.pure| const StringHash* { &VSP_ZONE } |]

vspLightmatrices :: Ptr StringHash
vspLightmatrices = [C.pure| const StringHash* { &VSP_LIGHTMATRICES } |]

vspSkinmatrices :: Ptr StringHash
vspSkinmatrices = [C.pure| const StringHash* { &VSP_SKINMATRICES } |]

vspVertexlights :: Ptr StringHash
vspVertexlights = [C.pure| const StringHash* { &VSP_VERTEXLIGHTS } |]

pspAmbientcolor :: Ptr StringHash
pspAmbientcolor = [C.pure| const StringHash* { &PSP_AMBIENTCOLOR } |]

pspCamerapos :: Ptr StringHash
pspCamerapos = [C.pure| const StringHash* { &PSP_CAMERAPOS } |]

pspDeltatime :: Ptr StringHash
pspDeltatime = [C.pure| const StringHash* { &PSP_DELTATIME } |]

pspDepthreconstruct :: Ptr StringHash
pspDepthreconstruct = [C.pure| const StringHash* { &PSP_DEPTHRECONSTRUCT } |]

pspElapsedtime :: Ptr StringHash
pspElapsedtime = [C.pure| const StringHash* { &PSP_ELAPSEDTIME } |]

pspFogcolor :: Ptr StringHash
pspFogcolor = [C.pure| const StringHash* { &PSP_FOGCOLOR } |]

pspFogparams :: Ptr StringHash
pspFogparams = [C.pure| const StringHash* { &PSP_FOGPARAMS } |]

pspGbufferinvsize :: Ptr StringHash
pspGbufferinvsize = [C.pure| const StringHash* { &PSP_GBUFFERINVSIZE } |]

pspLightcolor :: Ptr StringHash
pspLightcolor = [C.pure| const StringHash* { &PSP_LIGHTCOLOR } |]

pspLightdir :: Ptr StringHash
pspLightdir = [C.pure| const StringHash* { &PSP_LIGHTDIR } |]

pspLightpos :: Ptr StringHash
pspLightpos = [C.pure| const StringHash* { &PSP_LIGHTPOS } |]

pspMatdiffcolor :: Ptr StringHash
pspMatdiffcolor = [C.pure| const StringHash* { &PSP_MATDIFFCOLOR } |]

pspMatemissivecolor :: Ptr StringHash
pspMatemissivecolor = [C.pure| const StringHash* { &PSP_MATEMISSIVECOLOR } |]

pspMatenvmapcolor :: Ptr StringHash
pspMatenvmapcolor = [C.pure| const StringHash* { &PSP_MATENVMAPCOLOR } |]

pspMatspeccolor :: Ptr StringHash
pspMatspeccolor = [C.pure| const StringHash* { &PSP_MATSPECCOLOR } |]

pspNearclip :: Ptr StringHash
pspNearclip = [C.pure| const StringHash* { &PSP_NEARCLIP } |]

pspFarclip :: Ptr StringHash
pspFarclip = [C.pure| const StringHash* { &PSP_FARCLIP } |]

pspShadowcubeadjust :: Ptr StringHash
pspShadowcubeadjust = [C.pure| const StringHash* { &PSP_SHADOWCUBEADJUST } |]

pspShadowdepthfade :: Ptr StringHash
pspShadowdepthfade = [C.pure| const StringHash* { &PSP_SHADOWDEPTHFADE } |]

pspShadowintensity :: Ptr StringHash
pspShadowintensity = [C.pure| const StringHash* { &PSP_SHADOWINTENSITY } |]

pspShadowmapinvsize :: Ptr StringHash
pspShadowmapinvsize = [C.pure| const StringHash* { &PSP_SHADOWMAPINVSIZE } |]

pspShadowsplits :: Ptr StringHash
pspShadowsplits = [C.pure| const StringHash* { &PSP_SHADOWSPLITS } |]

pspLightmatrices :: Ptr StringHash
pspLightmatrices = [C.pure| const StringHash* { &PSP_LIGHTMATRICES } |]

pspVSMShadowParams :: Ptr StringHash
pspVSMShadowParams = [C.pure| const StringHash* { &PSP_VSMSHADOWPARAMS } |]

pspRoughness :: Ptr StringHash
pspRoughness = [C.pure| const StringHash* { &PSP_ROUGHNESS } |]

pspMetallic :: Ptr StringHash
pspMetallic = [C.pure| const StringHash* { &PSP_METALLIC } |]

-- | Scale calculation from bounding box diagonal.
dotScale :: Vector3
dotScale = unsafePerformIO $ peek =<< [C.exp| const Vector3* { &DOT_SCALE } |]
