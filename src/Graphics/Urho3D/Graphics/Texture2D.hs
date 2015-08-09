{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Texture2D(
    Texture2D
  , texture2DContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Texture2D
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> texture2DCntx <> textureContext <> stringHashContext)
C.include "<Urho3D/Graphics/Texture2D.h>"
C.using "namespace Urho3D"

texture2DContext :: C.Context 
texture2DContext = texture2DCntx <> textureContext

instance ResourceType Texture2D where 
  resourceType _ = unsafePerformIO $ [C.block| StringHash* { 
    static StringHash h = Texture2D::GetTypeStatic(); 
    return &h; 
    } |]

instance Parent Texture Texture2D  where 
  castToParent ptr = [C.pure| Texture* {(Texture*)$(Texture2D* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Texture2D* {(Texture2D*)$(Texture* ptr)} |]
    in if child == nullPtr then Nothing else Just child