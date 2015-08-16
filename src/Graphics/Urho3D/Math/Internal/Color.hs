{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graphics.Urho3D.Math.Internal.Color(
    Color(..)
  , colorCntx
  , HasRComp(..)
  , HasGComp(..)
  , HasBComp(..)
  , HasAComp(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Control.Lens 

data Color = Color {
  _colorRComp :: Float 
, _colorGComp :: Float 
, _colorBComp :: Float   
, _colorAComp :: Float   
} deriving (Eq, Show)
makeFields ''Color

colorCntx :: C.Context 
colorCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Color", [t| Color |])
    ]
  }