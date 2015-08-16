{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Urho3D.Graphics.Defs where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector3
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> stringHashContext <> vector3Context)
C.include "<Urho3D/Graphics/GraphicsDefs.h>"
C.using "namespace Urho3D"

-- | Primitive type
data PrimitiveType = 
    TriangleList
  | LineList
  | PointList
  | TriangleStrip
  | LineStrip
  | TriangleFan
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Geometry type
data GeometryType =
    GeomStatic
  | GeomSkinned
  | GeomInstanced
  | GeomBillboard
  | GeomStaticNoInstancing
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Blending mode
data BlendMode = 
    BlendReplace
  | BlendAdd
  | BlendMultiply
  | BlendAlpha
  | BlendAddAlpha
  | BlendPreMulAlpha
  | BlendInvDestAlpha
  | BlendSubtract
  | BlendSubtractAlpha
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Depht or stencil compare mode
data CompareMode = 
    CmpAlways
  | CmpEqual
  | CmpNotEqual
  | CmpLess
  | CmpLessEqual
  | CmpGreater
  | CmpGreaterEqual
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Culling mode
data CullMode = 
    CullNone
  | CullCCW
  | CullCW 
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Fill mode
data Fillmode =
    FillSolid
  | FillWireFrame
  | FillPoint
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Stencil operation
data StencilOp = 
    StencilKeep
  | StencilZero
  | StencilRef
  | StencilIncr
  | StencilDecr
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Vertex/index buffer lock state
data LockState = 
    LockNone
  | LockHardware
  | LockShadow
  | LockScratch
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Vertex elements
data VertexElement = 
    ElementPosition
  | ElementNormal
  | ElementColor
  | ElementTexCoord1
  | ElementTexCoord2
  | ElementCubeTexCoord1
  | ElementCubeTexCoord2
  | ElementTangent
  | ElementBlendWeights
  | ElementBlendIndices
  | ElementInstanceMatrix1
  | ElementInstanceMatrix2
  | ElementInstanceMatrix3
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Texture filtering mode
data TextureFilterMode =
    FilterNearest
  | FilterBiLinear
  | FilterTriLinear
  | FilterAnisotropic
  | FilterDefault
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Texture addressing mode
data TextureAddressMode = 
    AddressWrap
  | AddressMirror
  | AddressClamp
  | AddressBorder
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Texture coordinates
data TextureCoordinate =
    CoordU
  | CoordV
  | CoordW
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Texture usage types
data TextureUsage =
    TextureStatic
  | TextureDynamic
  | TextureRenderTarget
  | TextureDepthStencil
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Cube map faces
data CubeMapFace = 
    FacePositiveX 
  | FaceNegativeX
  | FacePositiveY
  | FaceNegativeY
  | FacePositiveZ
  | FaceNegativeZ
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Cubemap single image layout modes
data CubeMapLayout = 
    CubeMapLayoutHorizontal
  | CubeMapLayoutHorizontalNVIDIA
  | CubeMapLaoyutHorizontalCross 
  | CubeMapLayoutVerticalCross
  | CubeMapLayoutBlender
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Update mode fo render serface viewports
data RenderSurfaceUpdateMode =
    SurfaceManualUpdate
  | SurfaceUpdateVisible
  | SurfaceUpdateAlways
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Shader types
data ShaderType =
    ShaderVertex
  | ShaderFragment
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Shader parameter groups for determining need to update. On APIs that support constant buffers, these correspond to different constant buffers.
data ShaderParameterGroup = 
    SP'Frame 
  | SP'Camera
  | SP'Zone
  | SP'Light
  | SP'Material
  | SP'Object
  | SP'Custom
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Texture units
data TextureUnit =
    TU'Diffuse
  | TU'AlbedoBuffer
  | TU'Normal 
  | TU'NormalBuffer
  | TU'Specular
  | TU'Emissive
  | TU'Environment
  | TU'VolumeMap
  | TU'Custom1
  | TU'Custom2
  | TU'LightRamp
  | TU'LightShape
  | TU'ShadowMap
  | TU'FaceSelect
  | TU'InDirection
  | TU'DepthBuffer
  | TU'LightBuffer
  | TU'Zone
  deriving (Eq, Ord, Show, Bounded)

maxMatrialTextureUnits :: Integral a => a 
maxMatrialTextureUnits = 8

maxTextureUnits :: Integral a => a 
maxTextureUnits = 16

instance Enum TextureUnit where 
  toEnum i = case i of 
    0 -> TU'Diffuse
    1 -> TU'Normal
    2 -> TU'Specular
    3 -> TU'Emissive
    4 -> TU'Environment
    5 -> TU'VolumeMap
    6 -> TU'Custom1
    7 -> TU'Custom2
    8 -> TU'LightRamp
    9 -> TU'LightShape
    10 -> TU'ShadowMap
    11 -> TU'FaceSelect
    12 -> TU'InDirection
    13 -> TU'DepthBuffer
    14 -> TU'LightBuffer
    15 -> TU'Zone
    _ -> TU'Diffuse

  fromEnum e = case e of 
    TU'Diffuse -> 0
    TU'AlbedoBuffer -> 0
    TU'Normal -> 1
    TU'NormalBuffer -> 1
    TU'Specular -> 2
    TU'Emissive -> 3
    TU'Environment -> 4
    TU'VolumeMap -> 5
    TU'Custom1 -> 6
    TU'Custom2 -> 7
    TU'LightRamp -> 8
    TU'LightShape -> 9
    TU'ShadowMap -> 10
    TU'FaceSelect -> 11
    TU'InDirection -> 12
    TU'DepthBuffer -> 13
    TU'LightBuffer -> 14
    TU'Zone -> 15

-- | Billboard camera facing modes
data FaceCameraMode = 
    FC'None 
  | FC'RotateXYZ
  | FC'RotateY 
  | FC'LookAtXYZ
  | FC'LookAtY 
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Inbuilt shader parameters.
vspAmbientstartcolor :: Ptr StringHash
vspAmbientstartcolor = [C.pure| const StringHash* { &VSP_AMBIENTSTARTCOLOR } |]

vspAmbientendcolor :: Ptr StringHash
vspAmbientendcolor = [C.pure| const StringHash* { &VSP_AMBIENTENDCOLOR } |]

vspBillboardrot :: Ptr StringHash
vspBillboardrot = [C.pure| const StringHash* { &VSP_BILLBOARDROT } |]

vspCamerapos :: Ptr StringHash
vspCamerapos = [C.pure| const StringHash* { &VSP_CAMERAPOS } |]

vspCamerarot :: Ptr StringHash
vspCamerarot = [C.pure| const StringHash* { &VSP_CAMERAROT } |]

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

-- | Scale calculation from bounding box diagonal.
dotScale :: Vector3 
dotScale = unsafePerformIO $ peek =<< [C.exp| const Vector3* { &DOT_SCALE } |]

data Quality = 
    Quality'Low
  | Quality'Medium
  | Quality'High
  | Quality'Max
  deriving (Eq, Ord, Show, Bounded)

instance Enum Quality where 
  fromEnum q = case q of 
    Quality'Low -> 0 
    Quality'Medium -> 1 
    Quality'High -> 2 
    Quality'Max -> 15

  toEnum i = case i of 
    0 -> Quality'Low 
    1 -> Quality'Medium 
    2 -> Quality'High
    15 -> Quality'Max 
    _ -> Quality'Low 

data ShadowQuality = 
    ShadowQuality'Low16Bit 
  | ShadowQuality'Low24Bit
  | ShadowQuality'High16Bit
  | ShadowQuality'High24Bit
  deriving (Eq, Ord, Show, Bounded)

instance Enum ShadowQuality where 
  fromEnum q = case q of 
    ShadowQuality'Low16Bit -> 0
    ShadowQuality'Low24Bit -> 1
    ShadowQuality'High16Bit -> 2
    ShadowQuality'High24Bit -> 3

  toEnum i = case i of 
    0 -> ShadowQuality'Low16Bit
    1 -> ShadowQuality'Low24Bit
    2 -> ShadowQuality'High16Bit
    3 -> ShadowQuality'High24Bit
    _ -> ShadowQuality'Low16Bit

data Mask = 
    MaskNone
  | MaskPosition
  | MaskNormal
  | MaskColor
  | MaskTexCoord1
  | MaskTexCoord2 
  | MaskCubeTexCoord1
  | MaskCubeTexCoord2
  | MaskTangent
  | MaskBlendWeights
  | MaskBlendIndices
  | MaskInstanceMatrix1
  | MaskInstanceMatrix2
  | MaskInstanceMatrix3
  | MaskDefault 
  | MaskNoElement
  deriving (Eq, Ord, Show, Bounded)

instance Enum Mask where 
  fromEnum q = case q of 
    MaskNone -> 0x0
    MaskPosition -> 0x1
    MaskNormal -> 0x2
    MaskColor -> 0x4
    MaskTexCoord1 -> 0x8
    MaskTexCoord2  -> 0x10
    MaskCubeTexCoord1 -> 0x20
    MaskCubeTexCoord2 -> 0x40
    MaskTangent -> 0x80
    MaskBlendWeights -> 0x100
    MaskBlendIndices -> 0x200
    MaskInstanceMatrix1 -> 0x400
    MaskInstanceMatrix2 -> 0x800
    MaskInstanceMatrix3 -> 0x1000
    MaskDefault -> 0xffffffff
    MaskNoElement -> 0xffffffff

  toEnum i = case i of 
    0x0 -> MaskNone 
    0x1 -> MaskPosition
    0x2 -> MaskNormal
    0x4 -> MaskColor
    0x8 -> MaskTexCoord1
    0x10 -> MaskTexCoord2
    0x20 -> MaskCubeTexCoord1
    0x40 -> MaskCubeTexCoord2
    0x80 -> MaskTangent
    0x100 -> MaskBlendWeights
    0x200 -> MaskBlendIndices
    0x400 -> MaskInstanceMatrix1
    0x800 -> MaskInstanceMatrix2
    0x1000 -> MaskInstanceMatrix3
    0xffffffff -> MaskDefault
    _ -> MaskDefault

maxRenderTargets :: Integral a => a 
maxRenderTargets = 4

maxVertexStreams :: Integral a => a 
maxVertexStreams = 4

maxConstantRegisters :: Integral a => a 
maxConstantRegisters = 256

bitsPerComponent :: Integral a => a 
bitsPerComponent = 8