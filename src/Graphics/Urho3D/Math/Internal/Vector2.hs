{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graphics.Urho3D.Math.Internal.Vector2(
    Vector2(..)
  , IntVector2(..)
  , vector2Cntx
  , HasX(..)
  , HasY(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Control.Lens 

data Vector2 = Vector2 {
  _vector2X :: Float 
, _vector2Y :: Float  
} deriving (Eq, Ord, Show)
makeFields ''Vector2

data IntVector2 = IntVector2 {
  _intVector2X :: Int 
, _intVector2Y :: Int
} deriving (Eq, Ord, Show)
makeFields ''IntVector2

vector2Cntx :: C.Context 
vector2Cntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Vector2", [t| Vector2 |])
    , (C.TypeName "IntVector2", [t| IntVector2 |])
    ]
  }