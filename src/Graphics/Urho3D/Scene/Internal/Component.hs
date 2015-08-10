{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Scene.Internal.Component(
    Component
  , componentCntx
  , sharedComponentPtrCntx
  , SharedComponent
  , SharedComponentPtr(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Component

componentCntx :: C.Context 
componentCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Component", [t| Component |])
    ]
  }

sharedPtrImpl "Component" 