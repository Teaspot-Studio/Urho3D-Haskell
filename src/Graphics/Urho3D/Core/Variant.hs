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
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.Variant
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.HashMap
import Graphics.Urho3D.Math.StringHash
import Control.Monad.IO.Class
import Data.Monoid
import Foreign 
import Foreign.C.String 

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
instance Createable Variant where 
  type CreationOptions Variant = ()

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
  setVariant a ptr = do 
    str <- newCString a 
    [C.exp| void { *$(Variant* ptr) = $(char* str) } |]
  getVariant ptr = do 
    t <- variantType ptr 
    case t of 
      VariantString -> do 
       str <- peekCString =<< [C.exp| const char* { $(Variant* ptr)->GetString().CString() } |]
       return $ Just str
      _ -> return Nothing

-- | Creates new Variant with specified value inside
newVariant :: VariantStorable a => a -> IO (Ptr Variant)
newVariant val = do 
  ptr <- newEmptyVariant
  setVariant val ptr
  return ptr 

hashMap "StringHash" "Variant"