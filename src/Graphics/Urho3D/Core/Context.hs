{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.Context(
    Context
  , contextContext
  , contextCreateObject
  , contextRegisterFactory
  , contextRegisterFactoryCat
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.Context
import Graphics.Urho3D.Core.Internal.Object
import Graphics.Urho3D.Core.Internal.SharedObject
import Graphics.Urho3D.Core.Internal.ObjectFactory 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.C.String 

C.context (C.cppCtx <> contextCntx <> objectFactoryCntx <> objectCntx <> sharedObjectPtrCntx <> stringHashContext)
C.include "<Urho3D/Core/Context.h>"
C.using "namespace Urho3D"

C.verbatim "typedef SharedPtr<Object> SharedObject;"

contextContext :: C.Context 
contextContext = contextCntx 

newContext :: IO (Ptr Context)
newContext = [C.exp| Context* { new Context() } |]

deleteContext :: Ptr Context -> IO ()
deleteContext ptr = [C.exp| void { delete $(Context* ptr) } |]

instance Createable (Ptr Context) where 
  type CreationOptions (Ptr Context) = ()

  newObject _ = liftIO newContext
  deleteObject = liftIO . deleteContext

-- | Create an object by type hash. Return pointer to it or null if no factory found.
contextCreateObject :: (Parent Context a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to context
  -> Ptr StringHash -- ^ Type hash
  -> m (Maybe SharedObjectPtr)
contextCreateObject p phash = liftIO $ do 
  let ptr = parentPointer p 
  op <- wrapSharedObjectPtr =<< [C.exp| SharedObject* { new SharedPtr<Object>( $(Context* ptr)->CreateObject(*$(StringHash* phash)) ) } |]
  checkNullPtr' op return

-- | Register a factory for an object type.
contextRegisterFactory :: (Parent Context a, Pointer p a, Parent ObjectFactory b, Pointer pfactory b, MonadIO m) 
  => p -- ^ Pointer to context
  -> pfactory -- ^ Pointer to object factory
  -> m ()
contextRegisterFactory p pfactory = liftIO $ do 
  let ptr = parentPointer p 
      pfactory' = parentPointer pfactory
  [C.exp| void { $(Context* ptr)->RegisterFactory($(ObjectFactory* pfactory')) } |]

-- | Register a factory for an object type and specify the object category.
contextRegisterFactoryCat :: (Parent Context a, Pointer p a, Parent ObjectFactory b, Pointer pfactory b, MonadIO m) 
  => p -- ^ Pointer to context
  -> pfactory -- ^ Pointer to object factory
  -> String -- ^ Object category
  -> m ()
contextRegisterFactoryCat p pfactory str = liftIO $ withCString str $ \str' -> do 
  let ptr = parentPointer p 
      pfactory' = parentPointer pfactory
  [C.exp| void { $(Context* ptr)->RegisterFactory($(ObjectFactory* pfactory'), $(const char* str')) } |]
