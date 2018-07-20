module Graphics.Urho3D.Graphics.Internal.Camera(
    Camera
  , ViewOverride(..)
  , ViewOverrideFlags
  , cameraCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Data.Word
import GHC.Generics
import Graphics.Urho3D.Container.FlagSet

import qualified Data.Map as Map

data Camera

data ViewOverride =
    ViewOverrideNone
  | ViewOverrideLowMaterialQuality
  | ViewOverrideDisableShadows
  | ViewOverrideDisableOcclusion
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum ViewOverride where
  toEnum v = case v of
    0 -> ViewOverrideNone
    1 -> ViewOverrideLowMaterialQuality
    2 -> ViewOverrideDisableShadows
    4 -> ViewOverrideDisableOcclusion
    _ -> ViewOverrideNone
  {-# INLINE toEnum #-}

  fromEnum v = case v of
    ViewOverrideNone -> 0
    ViewOverrideLowMaterialQuality -> 1
    ViewOverrideDisableShadows -> 2
    ViewOverrideDisableOcclusion -> 4
  {-# INLINE fromEnum #-}

type ViewOverrideFlags = FlagSet Word32 ViewOverride

cameraCntx :: C.Context
cameraCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Camera", [t| Camera |])
    ]
  }
