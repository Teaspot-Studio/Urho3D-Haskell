{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.ObjectFactory(
    ObjectFactory
  , SharedObjectFactory
  , SharedObjectFactoryPtr(..)
  , objectFactoryContext
  , wrapSharedObjectFactoryPtr
  , factoryCreateObject
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.ObjectFactory
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid

C.context (C.cppCtx <> C.funConstCtx <> objectContext <> objectFactoryCntx <> contextContext <> stringHashContext <> sharedObjectFactoryPtrCntx)
C.include "<Urho3D/Core/Object.h>"
C.using "namespace Urho3D"

objectFactoryContext :: C.Context 
objectFactoryContext = objectContext <> stringHashContext <> objectFactoryCntx <> sharedObjectFactoryPtrCntx

instance AbstractType ObjectFactory

sharedPtr "ObjectFactory" 

C.verbatim "typedef SharedPtr<Object> SharedObject;"

-- | Create an object. Implemented by factory ascentors
factoryCreateObject :: (Parent ObjectFactory a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to object factory
  -> m (Maybe SharedObjectPtr) -- ^ Resulting object, Nothing if failed
factoryCreateObject p = liftIO $ do 
  let ptr = parentPointer p 
  op <- wrapSharedObjectPtr =<< [C.exp| SharedObject* { new SharedPtr<Object>($(ObjectFactory* ptr)->CreateObject()) } |]
  checkNullPtr' op return