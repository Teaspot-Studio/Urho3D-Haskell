{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.IO.Deserializer(
    Deserializer
  , deserializerContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.IO.Internal.Deserializer
import Data.Monoid

C.context (C.cppCtx <> deserializerCntx)
C.include "<Urho3D/IO/Deserializer.h>"
C.using "namespace Urho3D"

deserializerContext :: C.Context 
deserializerContext = deserializerCntx