module Graphics.Urho3D.Core.Internal.Variant(
    Variant
  , ResourceRef(..)
  , ResourceRefList(..)
  , HasObjectType(..)
  , HasObjectName(..)
  , HasObjectNames(..)
  , VariantType(..)
  , variantCntx
  , variantMapCntx
  , VariantMap
  , HashMapStringHashVariant
  , VectorVariant
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.HashMap
import Graphics.Urho3D.Math.StringHash
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Lens 

-- | Variable that supports a fixed set of types.
data Variant
data VectorVariant

-- | Typed resource reference.
data ResourceRef = ResourceRef {
  _resourceRefObjectType :: !StringHash -- ^ Object type
, _resourceRefObjectName :: !String -- ^ Object name
} deriving (Generic)

-- | List of typed resource references.
data ResourceRefList = ResourceRefList {
  _resourceRefListObjectType :: !StringHash -- ^ Object type
, _resourceRefListObjectNames :: ![String] -- ^ List of object names
} deriving (Generic)

makeFields ''ResourceRef
makeFields ''ResourceRefList
instance NFData ResourceRef
instance NFData ResourceRefList

variantCntx :: C.Context 
variantCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Variant", [t| Variant |])
    , (C.TypeName "VectorVariant", [t| VectorVariant |])
    , (C.TypeName "ResourceRef", [t| ResourceRef |])
    , (C.TypeName "ResourceRefList", [t| ResourceRefList |])
    ]
  } 

-- | Variant's supported types.
data VariantType = 
    VariantNone
  | VariantInt
  | VariantBool
  | VariantFloat
  | VariantVector2
  | VariantVector3
  | VariantVector4
  | VariantQuaternion
  | VariantColor
  | VariantString
  | VariantBuffer
  | VariantVoidPtr
  | VariantResourceRef
  | VariantResourceRefList
  | VariantVariantVector
  | VariantVariantMap
  | VariantIntRect
  | VariantIntVector2
  | VariantPtr
  | VariantMatrix3
  | VariantMatrix3x4
  | VariantMatrix4 
  | VariantDouble
  deriving (Show, Generic, Eq, Ord, Bounded, Enum)

hashMapImpl "StringHash" "Variant"

type VariantMap = HashMapStringHashVariant

variantMapCntx :: C.Context 
variantMapCntx = hashMapStringHashVariantCntx
