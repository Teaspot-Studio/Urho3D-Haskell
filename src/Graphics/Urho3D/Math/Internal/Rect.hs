{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graphics.Urho3D.Math.Internal.Rect(
    Rect(..)
  , IntRect(..)
  , rectCntx
  , HasMinPoint(..)
  , HasMaxPoint(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Vector2
import Control.Lens 

data Rect = Rect {
  _rectMinPoint :: Vector2 
, _rectMaxPoint :: Vector2  
}
makeFields ''Rect

data IntRect = IntRect {
  _intRectLeft :: Int 
, _intRectTop :: Int 
, _intRectRight :: Int 
, _intRectBottom :: Int
}
makeFields ''IntRect

rectCntx :: C.Context 
rectCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Rect", [t| Rect |])
    , (C.TypeName "IntRect", [t| IntRect |])
    ]
  }