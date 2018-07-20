module Graphics.Urho3D.Graphics.Internal.Defs where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

import Control.DeepSeq
import Control.Lens
import Data.Word
import GHC.Generics
import Graphics.Urho3D.Container.FlagSet

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
data FillMode =
    FillSolid
  | FillWireFrame
  | FillPoint
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData FillMode

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
    TypeInt
  | TypeFloat
  | TypeVector2
  | TypeVector3
  | TypeVector4
  | TypeUbyte4
  | TypeUbyte4Norm
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData VertexElementType

-- | Arbitrary vertex declaration element semantics.
data VertexElementSemantic =
    SEMPosition
  | SEMNormal
  | SEMBinormal
  | SEMTangent
  | SEMTexCoord
  | SEMColor
  | SEMBlendWeights
  | SEMBlendIndices
  | SEMObjectIndex
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData VertexElementSemantic

-- | Texture filtering mode
data TextureFilterMode =
    FilterNearest
  | FilterBiLinear
  | FilterTriLinear
  | FilterAnisotropic
  | FilterNearestAnisotropic
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
    TUDiffuse
  | TUAlbedoBuffer
  | TUNormal
  | TUNormalBuffer
  | TUSpecular
  | TUEmissive
  | TUEnvironment
  | TUVolumeMap
  | TUCustom1
  | TUCustom2
  | TULightRamp
  | TULightShape
  | TUShadowMap
  | TUFaceSelect
  | TUInDirection
  | TUDepthBuffer
  | TULightBuffer
  | TUZone
  deriving (Eq, Ord, Show, Bounded, Generic)

instance NFData TextureUnit

maxMatrialTextureUnits :: Integral a => a
maxMatrialTextureUnits = 8

maxTextureUnits :: Integral a => a
maxTextureUnits = 16

instance Enum TextureUnit where
  toEnum i = case i of
    0 -> TUDiffuse
    1 -> TUNormal
    2 -> TUSpecular
    3 -> TUEmissive
    4 -> TUEnvironment
    5 -> TUVolumeMap
    6 -> TUCustom1
    7 -> TUCustom2
    8 -> TULightRamp
    9 -> TULightShape
    10 -> TUShadowMap
    11 -> TUFaceSelect
    12 -> TUInDirection
    13 -> TUDepthBuffer
    14 -> TULightBuffer
    15 -> TUZone
    _ -> TUDiffuse
  {-# INLINE fromEnum #-}

  fromEnum e = case e of
    TUDiffuse -> 0
    TUAlbedoBuffer -> 0
    TUNormal -> 1
    TUNormalBuffer -> 1
    TUSpecular -> 2
    TUEmissive -> 3
    TUEnvironment -> 4
    TUVolumeMap -> 5
    TUCustom1 -> 6
    TUCustom2 -> 7
    TULightRamp -> 8
    TULightShape -> 9
    TUShadowMap -> 10
    TUFaceSelect -> 11
    TUInDirection -> 12
    TUDepthBuffer -> 13
    TULightBuffer -> 14
    TUZone -> 15
  {-# INLINE toEnum #-}

-- | Billboard camera facing modes
data FaceCameraMode =
    FCNone
  | FCRotateXYZ
  | FCRotateY
  | FCLookAtXYZ
  | FCLookAtY
  | FCDirection
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData FaceCameraMode

data ShadowQuality =
    ShadowQualitySimple16Bit
  | ShadowQualitySimple24Bit
  | ShadowQualityPCF16Bit
  | ShadowQualityPCF24Bit
  | ShadowQualityVSM
  | ShadowQualityBlurVSM
  deriving (Eq, Ord, Show, Bounded, Generic)

instance NFData ShadowQuality

instance Enum ShadowQuality where
  fromEnum q = case q of
    ShadowQualitySimple16Bit -> 0
    ShadowQualitySimple24Bit -> 1
    ShadowQualityPCF16Bit -> 2
    ShadowQualityPCF24Bit -> 3
    ShadowQualityVSM -> 4
    ShadowQualityBlurVSM -> 5
  {-# INLINE fromEnum #-}

  toEnum i = case i of
    0 -> ShadowQualitySimple16Bit
    1 -> ShadowQualitySimple24Bit
    2 -> ShadowQualityPCF16Bit
    3 -> ShadowQualityPCF24Bit
    4 -> ShadowQualityVSM
    5 -> ShadowQualityBlurVSM
    _ -> ShadowQualitySimple16Bit
  {-# INLINE toEnum #-}

data MaterialQuality =
    MaterialQualityLow
  | MaterialQualityMedium
  | MaterialQualityHigh
  | MaterialQualityMax
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance NFData MaterialQuality

instance Enum MaterialQuality where
  fromEnum v = case v of
    MaterialQualityLow    -> 0
    MaterialQualityMedium -> 1
    MaterialQualityHigh   -> 2
    MaterialQualityMax    -> 15
  {-# INLINE fromEnum #-}

  toEnum v = case v of
    0  -> MaterialQualityLow
    1  -> MaterialQualityMedium
    2  -> MaterialQualityHigh
    15 -> MaterialQualityMax
    _  -> MaterialQualityMax
  {-# INLINE toEnum #-}

data VertexMask =
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

type VertexMaskFlags = FlagSet Word32 VertexMask

instance NFData VertexMask

instance Enum VertexMask where
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
