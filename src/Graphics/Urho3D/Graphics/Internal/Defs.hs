module Graphics.Urho3D.Graphics.Internal.Defs where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

import Control.DeepSeq
import Control.Lens
import Data.Word
import GHC.Generics

-- | Primitive type
data PrimitiveType =
    TriangleList
  | LineList
  | PointList
  | TriangleStrip
  | LineStrip
  | TriangleFan
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData PrimitiveType

-- | Geometry type
data GeometryType =
    GeomStatic
  | GeomSkinned
  | GeomInstanced
  | GeomBillboard
  | GeomDirBillboard
  | GeomStaticNoInstancing
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData GeometryType

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
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData BlendMode

-- | Depht or stencil compare mode
data CompareMode =
    CmpAlways
  | CmpEqual
  | CmpNotEqual
  | CmpLess
  | CmpLessEqual
  | CmpGreater
  | CmpGreaterEqual
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData CompareMode

-- | Culling mode
data CullMode =
    CullNone
  | CullCCW
  | CullCW
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData CullMode

-- | Fill mode
data Fillmode =
    FillSolid
  | FillWireFrame
  | FillPoint
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData Fillmode

-- | Stencil operation
data StencilOp =
    StencilKeep
  | StencilZero
  | StencilRef
  | StencilIncr
  | StencilDecr
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData StencilOp

-- | Vertex/index buffer lock state
data LockState =
    LockNone
  | LockHardware
  | LockShadow
  | LockScratch
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData LockState

-- | Vertex elements
data LegacyVertexElement =
    LegacyElementPosition
  | LegacyElementNormal
  | LegacyElementColor
  | LegacyElementTexCoord1
  | LegacyElementTexCoord2
  | LegacyElementCubeTexCoord1
  | LegacyElementCubeTexCoord2
  | LegacyElementTangent
  | LegacyElementBlendWeights
  | LegacyElementBlendIndices
  | LegacyElementInstanceMatrix1
  | LegacyElementInstanceMatrix2
  | LegacyElementInstanceMatrix3
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData LegacyVertexElement

-- | Arbitrary vertex declaration element datatypes.
data VertexElementType =
    Type'Int
  | Type'Float
  | Type'Vector2
  | Type'Vector3
  | Type'Vector4
  | Type'Ubyte4
  | Type'Ubyte4Norm
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData VertexElementType

-- | Arbitrary vertex declaration element semantics.
data VertexElementSemantic =
    SEM'Position
  | SEM'Normal
  | SEM'Binormal
  | SEM'Tangent
  | SEM'TexCoord
  | SEM'Color
  | SEM'BlendWeights
  | SEM'BlendIndices
  | SEM'ObjectIndex
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData VertexElementSemantic

-- | Texture filtering mode
data TextureFilterMode =
    FilterNearest
  | FilterBiLinear
  | FilterTriLinear
  | FilterAnisotropic
  | FilterDefault
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData TextureFilterMode

-- | Texture addressing mode
data TextureAddressMode =
    AddressWrap
  | AddressMirror
  | AddressClamp
  | AddressBorder
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData TextureAddressMode

-- | Texture coordinates
data TextureCoordinate =
    CoordU
  | CoordV
  | CoordW
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData TextureCoordinate

-- | Texture usage types
data TextureUsage =
    TextureStatic
  | TextureDynamic
  | TextureRenderTarget
  | TextureDepthStencil
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData TextureUsage

-- | Cube map faces
data CubeMapFace =
    FacePositiveX
  | FaceNegativeX
  | FacePositiveY
  | FaceNegativeY
  | FacePositiveZ
  | FaceNegativeZ
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData CubeMapFace

-- | Cubemap single image layout modes
data CubeMapLayout =
    CubeMapLayoutHorizontal
  | CubeMapLayoutHorizontalNVIDIA
  | CubeMapLaoyutHorizontalCross
  | CubeMapLayoutVerticalCross
  | CubeMapLayoutBlender
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData CubeMapLayout

-- | Update mode fo render serface viewports
data RenderSurfaceUpdateMode =
    SurfaceManualUpdate
  | SurfaceUpdateVisible
  | SurfaceUpdateAlways
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData RenderSurfaceUpdateMode

-- Shader types
data ShaderType =
    ShaderVertex
  | ShaderFragment
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData ShaderType

-- | Shader parameter groups for determining need to update. On APIs that support constant buffers, these correspond to different constant buffers.
data ShaderParameterGroup =
    SP'Frame
  | SP'Camera
  | SP'Zone
  | SP'Light
  | SP'Material
  | SP'Object
  | SP'Custom
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData ShaderParameterGroup

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
  deriving (Eq, Ord, Show, Bounded, Generic)

instance NFData TextureUnit

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
  | FC'Direction
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData FaceCameraMode

data Quality =
    Quality'Low
  | Quality'Medium
  | Quality'High
  | Quality'Max
  deriving (Eq, Ord, Show, Bounded, Generic)

instance NFData Quality

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
    ShadowQuality'Simple16Bit
  | ShadowQuality'Simple24Bit
  | ShadowQuality'PCF16Bit
  | ShadowQuality'PCF24Bit
  | ShadowQuality'VSM
  | ShadowQuality'BlurVSM
  deriving (Eq, Ord, Show, Bounded, Generic)

instance NFData ShadowQuality

instance Enum ShadowQuality where
  fromEnum q = case q of
    ShadowQuality'Simple16Bit -> 0
    ShadowQuality'Simple24Bit -> 1
    ShadowQuality'PCF16Bit -> 2
    ShadowQuality'PCF24Bit -> 3
    ShadowQuality'VSM -> 4
    ShadowQuality'BlurVSM -> 5

  toEnum i = case i of
    0 -> ShadowQuality'Simple16Bit
    1 -> ShadowQuality'Simple24Bit
    2 -> ShadowQuality'PCF16Bit
    3 -> ShadowQuality'PCF24Bit
    4 -> ShadowQuality'VSM
    5 -> ShadowQuality'BlurVSM
    _ -> ShadowQuality'Simple16Bit

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
  deriving (Eq, Ord, Show, Bounded, Generic)

instance NFData Mask

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

-- | Vertex element description for arbitrary vertex declarations.
data VertexElement = VertexElement {
  _vertexElementElementType :: VertexElementType -- ^ Data type of element
, _vertexElementSemantic :: VertexElementSemantic -- ^ Semantic of element
, _vertexElementIndex :: Word8 -- ^ Semantic index of element, for example multi-texcoords.
, _vertexElementPerInstance :: Bool -- ^ Per-instance flag.
, _vertexElementOffset :: Word -- ^ Offset of element from vertex start. Filled by VertexBuffer once the vertex declaration is built.
} deriving (Show, Eq)


makeFields ''VertexElement

graphDefsCntx :: C.Context
graphDefsCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "VertexElement", [t| VertexElement |])
    ]
  }
