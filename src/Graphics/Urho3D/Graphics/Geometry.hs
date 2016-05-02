{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Geometry(
    Geometry
  , geometryContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Geometry
import Data.Monoid

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> geometryCntx <> objectContext)
C.include "<Urho3D/Graphics/Geometry.h>"
C.using "namespace Urho3D"

geometryContext :: C.Context 
geometryContext = geometryCntx

deriveParent ''Object ''Geometry