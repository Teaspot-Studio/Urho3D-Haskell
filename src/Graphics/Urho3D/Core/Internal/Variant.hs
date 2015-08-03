{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Graphics.Urho3D.Core.Internal.Variant(
    Variant
  , VariantType(..)
  , variantCntx
  , variantMapCntx
  , VariantMap
  , HashMapStringHashVariant
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.HashMap
import qualified Data.Map as Map
import GHC.Generics (Generic)

data Variant

variantCntx :: C.Context 
variantCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Variant", [t| Variant |])
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
