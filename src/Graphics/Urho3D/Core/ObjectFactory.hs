{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.ObjectFactory(
    ObjectFactory
  , SharedObjectFactory
  , SharedObjectFactoryPtr(..)
  , objectFactoryContext
  , wrapSharedObjectFactoryPtr
  , factoryCreateObject
  , factoryGetContext
  , factoryGetTypeInfo
  , factoryGetTypeName
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.ObjectFactory
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.TypeInfo
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> C.funConstCtx <> objectContext <> objectFactoryCntx <> contextContext <> stringHashContext <> sharedObjectFactoryPtrCntx <> typeInfoContext <> stringContext)
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

-- | Return execution context.
factoryGetContext :: (Parent ObjectFactory a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to object factory
  -> m (Ptr Context) -- ^ Considering non-null (will throw otherwise)
factoryGetContext p = liftIO $ do 
  let ptr = parentPointer p 
  guardNullPtr =<< [C.exp| Context* {$(ObjectFactory* ptr)->GetContext()} |]

-- | Return type info of objects created by this factory.
factoryGetTypeInfo :: (Parent ObjectFactory a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to object factory
  -> m (Ptr TypeInfo) -- ^ Considering non-null (will throw otherwise)
factoryGetTypeInfo p = liftIO $ do 
  let ptr = parentPointer p 
  guardNullPtr =<< [C.exp| const TypeInfo* {$(ObjectFactory* ptr)->GetTypeInfo()} |]

-- | Return type name of objects created by this factory.
factoryGetTypeName :: (Parent ObjectFactory a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to object factory
  -> m String
factoryGetTypeName p = liftIO $ do 
  let ptr = parentPointer p 
  loadConstUrhoString =<< [C.exp| const String* {&$(ObjectFactory* ptr)->GetTypeName()} |]
