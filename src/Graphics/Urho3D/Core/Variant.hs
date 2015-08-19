{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.Variant(
    Variant
  , VariantType
  , variantContext
  , variantType
  , VariantStorable(..)
  , newVariant
  , newVariantMaybe
  , withVariant
  , withVariantMaybe
  , VariantMap
  , HashMapStringHashVariant
  , variantMapGet
  , variantMapGet'
  , variantMapGetDefault
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C
import qualified Data.Text as T 

import Control.DeepSeq
import Data.Maybe (fromMaybe)
import Data.Monoid
import Foreign 
import Foreign.C.String 
import Graphics.Urho3D.Container.HashMap
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Internal.Variant
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.Rect 
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Monad

C.context (C.cppCtx <> variantCntx <> variantMapCntx <> stringHashContext <> contextContext <> stringContext <> vector2Context <> vector3Context <> rectContext)
C.include "<Urho3D/Core/Variant.h>"
C.using "namespace Urho3D"

variantContext :: C.Context 
variantContext = variantCntx <> variantMapCntx <> stringHashContext

newEmptyVariant :: IO (Ptr Variant)
newEmptyVariant = [C.exp| Variant* { new Variant() } |]

deleteVariant :: Ptr Variant -> IO ()
deleteVariant ptr = [C.exp| void { delete $(Variant* ptr) } |]

-- | Creates empty variant, for extended API view @VariantStorable@
instance Createable (Ptr Variant) where 
  type CreationOptions (Ptr Variant) = ()

  newObject _ = liftIO newEmptyVariant
  deleteObject = liftIO . deleteVariant

-- | Returns internal variant type
variantType :: Ptr Variant -> IO VariantType 
variantType ptr = toEnum.fromIntegral <$> [C.exp| int { (int)$(Variant* ptr)->GetType() } |]

-- | Extended API to create, set and get variant values of various types
class VariantStorable a where 
  setVariant :: MonadIO m => a -> Ptr Variant -> m ()
  getVariant :: MonadIO m => Ptr Variant -> m (Maybe a)

instance VariantStorable Bool where 
  setVariant a ptr = let val = fromBool a
    in liftIO [C.exp| void { *$(Variant* ptr) = $(int val) } |]
  getVariant ptr = liftIO $ do
    t <- variantType ptr 
    case t of 
      VariantBool -> Just . toBool <$> [C.exp| int { (int)$(Variant* ptr)->GetBool() } |]
      _ -> return Nothing

instance VariantStorable String where 
  setVariant a ptr = liftIO $ withCString a $ \str ->
    [C.exp| void { *$(Variant* ptr) = $(char* str) } |]
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantString -> do 
       str <- loadUrhoString =<< [C.exp| String* { new String($(Variant* ptr)->GetString()) } |]
       return $ Just str
      _ -> return Nothing

instance VariantStorable T.Text where 
  setVariant a ptr = liftIO $ textAsPtrW32 a $ \str ->
    [C.exp| void { *$(Variant* ptr) = String($(wchar_t* str)) } |]
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantString -> do 
       str <- loadUrhoText =<< [C.exp| String* { new String($(Variant* ptr)->GetString()) } |]
       return $ Just str
      _ -> return Nothing

instance VariantStorable Int where 
  setVariant a ptr = liftIO [C.exp| void { *$(Variant* ptr) = $(int a') } |]
    where a' = fromIntegral a 
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantInt -> do 
       v <- fromIntegral <$> [C.exp| int { $(Variant* ptr)->GetInt() } |]
       return $ Just v
      _ -> return Nothing

instance VariantStorable Float where 
  setVariant a ptr = liftIO [C.exp| void { *$(Variant* ptr) = $(float a') } |]
    where a' = realToFrac a 
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantFloat -> do 
       v <- realToFrac <$> [C.exp| float { $(Variant* ptr)->GetFloat() } |]
       return $ Just v
      _ -> return Nothing

instance VariantStorable Double where 
  setVariant a ptr = liftIO [C.exp| void { *$(Variant* ptr) = $(double a') } |]
    where a' = realToFrac a 
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantDouble -> do 
       v <- realToFrac <$> [C.exp| double { $(Variant* ptr)->GetDouble() } |]
       return $ Just v
      _ -> return Nothing

instance VariantStorable (Ptr a) where 
  setVariant a ptr = liftIO [C.exp| void { *$(Variant* ptr) = $(void* a') } |]
    where a' = castPtr a 
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantPtr -> do 
        v <- [C.exp| void* { $(Variant* ptr)->GetVoidPtr() } |]
        return $ Just $ castPtr v 
      _ -> return Nothing 

instance VariantStorable Vector2 where 
  setVariant v ptr = liftIO $ with v $ \v' -> [C.exp| void { *$(Variant* ptr) = *$(Vector2* v') } |]
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantVector2 -> do 
        v <- peek =<< [C.exp| const Vector2* { &$(Variant* ptr)->GetVector2() } |]
        return $ Just v
      _ -> return Nothing 

instance VariantStorable Vector3 where 
  setVariant v ptr = liftIO $ with v $ \v' -> [C.exp| void { *$(Variant* ptr) = *$(Vector3* v') } |]
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantVector3 -> do 
        v <- peek =<< [C.exp| const Vector3* { &$(Variant* ptr)->GetVector3() } |]
        return $ Just v
      _ -> return Nothing 

instance VariantStorable IntVector2 where 
  setVariant v ptr = liftIO $ with v $ \v' -> [C.exp| void { *$(Variant* ptr) = *$(IntVector2* v') } |]
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantIntVector2 -> do 
        v <- peek =<< [C.exp| const IntVector2* { &$(Variant* ptr)->GetIntVector2() } |]
        return $ Just v
      _ -> return Nothing 

instance VariantStorable IntRect where 
  setVariant v ptr = liftIO $ with v $ \v' -> [C.exp| void { *$(Variant* ptr) = *$(IntRect* v') } |]
  getVariant ptr = liftIO $ do 
    t <- variantType ptr 
    case t of 
      VariantIntRect -> do 
        v <- peek =<< [C.exp| const IntRect* { &$(Variant* ptr)->GetIntRect() } |]
        return $ Just v
      _ -> return Nothing 

-- | Creates new Variant with specified value inside
newVariant :: (MonadIO m, VariantStorable a) => a -> m (Ptr Variant)
newVariant val = liftIO $ do 
  ptr <- newEmptyVariant
  setVariant val ptr
  return ptr 

-- | Creates new Variant with specified value inside or empty variant
newVariantMaybe :: (MonadIO m, VariantStorable a) => Maybe a -> m (Ptr Variant)
newVariantMaybe (Just val) = newVariant val 
newVariantMaybe Nothing = liftIO newEmptyVariant

-- | Performs action with filled variant and after deletes it
withVariant :: (MonadIO m, MonadMask m, VariantStorable a) => a -> (Ptr Variant -> m b) -> m b 
withVariant a = bracket (newVariant a) deleteObject 

-- | Performs action with filled variant and after deletes it, creates empty variant for Nothing
withVariantMaybe :: (MonadIO m, MonadMask m, VariantStorable a) => Maybe a -> (Ptr Variant -> m b) -> m b 
withVariantMaybe a = bracket (newVariantMaybe a) deleteObject 

hashMap "StringHash" "Variant"

-- | Wrapper that helps to read values from @VariantMap@
variantMapGet :: (NFData a, VariantStorable a) => Ptr VariantMap -> String -> IO (Maybe a)
variantMapGet mp key = withObject' key $ \keyHash -> do
  variantM <- hashMapLookup keyHash mp
  join <$> whenJust variantM getVariant

-- | Wrapper that helps to read values from @VariantMap@
variantMapGet' :: VariantStorable a => Ptr VariantMap -> Ptr StringHash -> IO (Maybe a)
variantMapGet' mp keyHash = do
  variantM <- hashMapLookup keyHash mp
  join <$> whenJust variantM getVariant

-- | Same as @variantMapGet'@ but returns default value if not found
variantMapGetDefault :: VariantStorable a => Ptr VariantMap -> a -> Ptr StringHash -> IO a
variantMapGetDefault mp defValue keyHash = do
  variantM <- hashMapLookup keyHash mp
  fromMaybe defValue . join <$> whenJust variantM getVariant