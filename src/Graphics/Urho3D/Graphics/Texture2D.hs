{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Texture2D(
    Texture2D
  , texture2DContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Texture2D
import Graphics.Urho3D.Math.StringHash
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> texture2DCntx <> textureContext <> stringHashContext <> objectContext <> resourceContext)
C.include "<Urho3D/Graphics/Texture2D.h>"
C.using "namespace Urho3D"

texture2DContext :: C.Context
texture2DContext = texture2DCntx <> textureContext

instance ResourceType Texture2D where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Texture2D::GetTypeStatic().Value() } |]

deriveParents [''Object, ''Resource, ''Texture] ''Texture2D
