module Graphics.Urho3D.Core.Internal.CustomFactory(
    CustomFactory
  , customFactoryCntx
  , sharedCustomFactoryPtrCntx
  , SharedCustomFactory
  , SharedCustomFactoryPtr(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr

import qualified Data.Map as Map

data CustomFactory

customFactoryCntx :: C.Context 
customFactoryCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "CustomFactory", [t| CustomFactory |])
    ]
  } 

sharedPtrImpl "CustomFactory"