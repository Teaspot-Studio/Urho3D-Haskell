{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.IO.Serializer(
    Serializer
  , serializerContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.IO.Internal.Serializer
import Data.Monoid

C.context (C.cppCtx <> serializerCntx)
C.include "<Urho3D/IO/Serializer.h>"
C.using "namespace Urho3D"

serializerContext :: C.Context 
serializerContext = serializerCntx