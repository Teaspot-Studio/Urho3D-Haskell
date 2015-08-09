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
  , VariantMap
  , HashMapStringHashVariant
  , variantMapGet
  , variantMapGet'
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.Variant
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.HashMap
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.C.String 
import Control.DeepSeq

C.context (C.cppCtx <> variantCntx <> variantMapCntx <> stringHashContext <> contextContext)
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
  setVariant :: a -> Ptr Variant -> IO ()
  getVariant :: Ptr Variant -> IO (Maybe a)

instance VariantStorable Bool where 
  setVariant a ptr = let val = if a then 1 else 0
    in [C.exp| void { *$(Variant* ptr) = $(int val) } |]
  getVariant ptr = do
    t <- variantType ptr 
    case t of 
      VariantBool -> Just.(/=0) <$> [C.exp| int { (int)$(Variant* ptr)->GetBool() } |]
      _ -> return Nothing

instance VariantStorable String where 
  setVariant a ptr = withCString a $ \str ->
    [C.exp| void { *$(Variant* ptr) = $(char* str) } |]
  getVariant ptr = do 
    t <- variantType ptr 
    case t of 
      VariantString -> do 
       str <- peekCString =<< [C.exp| const char* { $(Variant* ptr)->GetString().CString() } |]
       return $ Just str
      _ -> return Nothing

instance VariantStorable Int where 
  setVariant a ptr = [C.exp| void { *$(Variant* ptr) = $(int a') } |]
    where a' = fromIntegral a 
  getVariant ptr = do 
    t <- variantType ptr 
    case t of 
      VariantInt -> do 
       v <- fromIntegral <$> [C.exp| int { $(Variant* ptr)->GetInt() } |]
       return $ Just v
      _ -> return Nothing

instance VariantStorable Float where 
  setVariant a ptr = [C.exp| void { *$(Variant* ptr) = $(float a') } |]
    where a' = realToFrac a 
  getVariant ptr = do 
    t <- variantType ptr 
    case t of 
      VariantFloat -> do 
       v <- realToFrac <$> [C.exp| float { $(Variant* ptr)->GetFloat() } |]
       return $ Just v
      _ -> return Nothing

instance VariantStorable Double where 
  setVariant a ptr = [C.exp| void { *$(Variant* ptr) = $(double a') } |]
    where a' = realToFrac a 
  getVariant ptr = do 
    t <- variantType ptr 
    case t of 
      VariantDouble -> do 
       v <- realToFrac <$> [C.exp| double { $(Variant* ptr)->GetDouble() } |]
       return $ Just v
      _ -> return Nothing

instance VariantStorable (Ptr a) where 
  setVariant a ptr = [C.exp| void { *$(Variant* ptr) = $(void* a') } |]
    where a' = castPtr a 
  getVariant ptr = do 
    t <- variantType ptr 
    case t of 
      VariantPtr -> do 
        v <- [C.exp| void* { $(Variant* ptr)->GetVoidPtr() } |]
        return $ Just $ castPtr v 
      _ -> return Nothing 

-- | Creates new Variant with specified value inside
newVariant :: VariantStorable a => a -> IO (Ptr Variant)
newVariant val = do 
  ptr <- newEmptyVariant
  setVariant val ptr
  return ptr 

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