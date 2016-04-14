module Graphics.Urho3D.Core.Internal.ObjectFactory(
    ObjectFactory
  , objectFactoryCntx
  , sharedObjectFactoryPtrCntx
  , SharedObjectFactory
  , SharedObjectFactoryPtr(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr

import qualified Data.Map as Map

data ObjectFactory

objectFactoryCntx :: C.Context 
objectFactoryCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "ObjectFactory", [t| ObjectFactory |])
    ]
  } 

sharedPtrImpl "ObjectFactory"