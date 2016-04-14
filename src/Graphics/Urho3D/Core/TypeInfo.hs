{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.TypeInfo(
    TypeInfo
  , typeInfoContext
  , newTypeInfo
  , IsTypeOf(..)
  , typeInfoGetType
  , typeInfoGetTypeName
  , typeInfoGetBaseTypeInfo
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.TypeInfo
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.Concurrent as FC 
import Foreign.C.String 

C.context (C.cppCtx <> C.funConstCtx <> objectContext <> typeInfoCntx <> contextContext <> stringHashContext <> stringContext)
C.include "<Urho3D/Core/Object.h>"
C.using "namespace Urho3D"

typeInfoContext :: C.Context 
typeInfoContext = objectContext <> stringHashContext <> typeInfoCntx

newTypeInfo :: MonadIO m => String -- ^ typeName
  -> Ptr TypeInfo -- ^ baseTypeInfo
  -> m (Ptr TypeInfo)
newTypeInfo s ptr = liftIO $ withCString s $ \s' -> 
  [C.exp| TypeInfo* {new TypeInfo($(const char* s'), $(TypeInfo* ptr))} |]

deleteTypeInfo :: Ptr TypeInfo -> IO ()
deleteTypeInfo ptr = [C.exp| void {delete $(TypeInfo* ptr)} |]

instance Createable (Ptr TypeInfo) where 
  type CreationOptions (Ptr TypeInfo) = (String, Ptr TypeInfo) 

  newObject = uncurry newTypeInfo
  deleteObject = liftIO . deleteTypeInfo

class IsTypeOf a where 
  -- | Check current type is type of specified type.
  typeInfoIsTypeOf :: (Parent TypeInfo t, Pointer p t, MonadIO m)
    => p -- ^ Pointer to type info
    -> Ptr a -- ^ type representation
    -> m Bool 

instance IsTypeOf StringHash where 
  typeInfoIsTypeOf p sh = liftIO $ do 
    let ptr = parentPointer p 
    toBool <$> [C.exp| int { $(TypeInfo* ptr)->IsTypeOf(*$(StringHash* sh)) } |]

instance IsTypeOf TypeInfo where 
  typeInfoIsTypeOf p sh = liftIO $ do 
    let ptr = parentPointer p 
    toBool <$> [C.exp| int { $(TypeInfo* ptr)->IsTypeOf($(const TypeInfo* sh)) } |]

-- | Return type
typeInfoGetType :: (Parent TypeInfo a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to type info 
  -> m (ForeignPtr StringHash) 
typeInfoGetType p = liftIO $ do 
  let ptr = parentPointer p 
  rp <- guardNullPtr =<< [C.exp| StringHash* { 
      new StringHash($(TypeInfo* ptr)->GetType()) } |]
  FC.newForeignPtr rp $ deleteStringHash rp

-- | Return type name.
typeInfoGetTypeName :: (Parent TypeInfo a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to type info 
  -> m String 
typeInfoGetTypeName p = liftIO $ do 
  let ptr = parentPointer p 
  loadConstUrhoString =<< [C.exp| const String* { &$(TypeInfo* ptr)->GetTypeName() } |]

-- | Return base type info.
typeInfoGetBaseTypeInfo :: (Parent TypeInfo a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to type info 
  -> m (Maybe (Ptr TypeInfo))
typeInfoGetBaseTypeInfo p = liftIO $ do 
  let ptr = parentPointer p 
  rp <- [C.exp| const TypeInfo* { $(TypeInfo* ptr)->GetBaseTypeInfo() } |]
  checkNullPtr' rp return