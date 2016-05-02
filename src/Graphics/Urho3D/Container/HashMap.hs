module Graphics.Urho3D.Container.HashMap(
    HashMap(..)
  , peekForeignHashMap
  , hashMap
  , hashMapPOD
  , hashMapImpl
  ) where 

import Graphics.Urho3D.Template
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Foreign

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import qualified Data.Map as Map

import Data.Hashable
import Data.IORef 
import qualified Data.HashMap.Strict as H 

-- | Operations that all templated hash maps should support
class HashMap m k v | m -> k, m -> v where 
  hashMapInsert :: k -> v -> Ptr m -> IO ()
  hashMapLookup :: k -> Ptr m -> IO (Maybe v)
  hashMapForeach :: Ptr m -> (k -> v -> IO ()) -> IO ()

-- | Convert foreign hash map into old plain hashmap
peekForeignHashMap :: (HashMap hm k v, Hashable k, Eq k, MonadIO m) => Ptr hm -> m (H.HashMap k v)
peekForeignHashMap ptr = liftIO $ do 
  ref <- newIORef H.empty
  hashMapForeach ptr $ \k v -> modifyIORef' ref $ H.insert k v 
  readIORef ref 

-- | Makes public API of HashMap<K, V> for given type
-- Makes following symbols:
-- newHashMapKV
-- deleteHashMapKV
-- instance Createable HashMapKV
--
-- Depends on symbols from @hashMapImpl@.
--
-- Note: if you get something like 'HashMapKV isn't defined' check if you added hashMapImpl in your
-- local context.
-- Note: if you get 'unexpected "f"', include C.funConstCtx in current context.
hashMap :: String -> String -> DecsQ
hashMap key value = do 
  typedef <- C.verbatim $ "typedef HashMap< " ++ key ++ ", " ++ value ++ " >" ++ hashMapName ++ ";"
  body <- sequence [
      newHashMapKV ^:: [t| IO (Ptr $hashMapType) |]
    , newHashMapKV ^= quoteExp C.exp (hashMapName ++ "* { new " ++ hashMapName ++ "()}")

    , deleteHashMapKV ^:: [t| Ptr $hashMapType -> IO () |]
    , mkFunc1 deleteHashMapKV "ptr" $ \ptrName -> 
        quoteExp C.exp ("void { delete $(" ++ hashMapName ++ "* " ++ show ptrName ++ ")}")
    ]
  createable <- [d| 
      instance Createable (Ptr $hashMapType) where 
        type CreationOptions (Ptr $hashMapType) = ()

        newObject _ = liftIO $(varE $ mkName newHashMapKV)
        deleteObject = liftIO . $(varE $ mkName deleteHashMapKV)
      |]
  hashMapClass <- [d|
      instance HashMap $hashMapType (Ptr $keyType) (Ptr $valueType) where
        hashMapInsert _k _v _m = $(quoteExp C.exp ("void { $(" ++ hashMapName ++ "* _m)->Insert(" ++ pairKV ++ "(*$(" ++ key ++ "* _k), *$(" ++ value ++ "* _v))) }"))
        hashMapLookup _k _m = do
          _v <- $(quoteExp C.exp (value ++ "* { $(" ++ hashMapName ++ "* _m)->Contains(*$(" ++ key ++ "* _k)) ? &(*$(" ++ hashMapName ++ "* _m))[*$(" ++ key ++ "* _k)] : NULL }"))
          checkNullPtr' _v return
        hashMapForeach _m _handler = do 
          $(quoteExp C.block ( "void {\n"
              ++ "HashMap<"++key++", "++value++">::Iterator it = $(" ++ hashMapName ++ "* _m)->Begin();\n"
              ++ "while(it != $(" ++ hashMapName ++ "* _m)->End()) {\n"
              ++ "  HashMap<"++key++", "++value++">::KeyValue kv = *it;\n"
              ++ "  $funConst:(void (*_handler)(const "++key++"*, "++value++"*))(&kv.first_, &kv.second_);\n"
              ++ "}"
            ++ "}"
            ))
      |]
  return $ typedef ++ body ++ createable ++ hashMapClass
  where 
  hashMapName = "HashMap" ++ key ++ value
  hashMapType = conT $ mkName hashMapName
  keyType = conT $ mkName key 
  valueType = conT $ mkName value 
  pairKV = "Pair< " ++ key ++ ", " ++ value ++ " >"
  newHashMapKV = "newHashMap" ++ key ++ value
  deleteHashMapKV = "deleteHashMap" ++ key ++ value

-- | Makes internal representation of HashMap for given key-value types
-- Makes following symbols:
-- data HashMapKV
-- hashMapKVCntx :: C.Context
hashMapImpl :: String -> String -> DecsQ 
hashMapImpl key value = do 
  sequence [
      return $ DataD [] (mkName hashMapName) [] [] []

    , hashMapCntx ^:: [t| C.Context |]
    , hashMapCntx ^= [e| mempty { 
        C.ctxTypesTable = Map.fromList [
          (C.TypeName $cTType, $cntxTType)
        ]
      } |]
    ]
  where
  hashMapName = "HashMap" ++ key ++ value
  hashMapCntx = "hashMap" ++ key ++ value ++ "Cntx"
  cTType = return $ LitE $ StringL hashMapName
  cntxTType = [e| return $ ConT $ mkName $cTType |]

-- | Makes public API of HashMap<K, V> for given type
-- Makes following symbols:
-- newHashMapKV
-- deleteHashMapKV
-- instance Createable HashMapKV
--
-- Depends on symbols from @hashMapImpl@.
-- 
-- Note: You need 'Storable' instance for K and V. 
-- Note: if you get something like 'HashMapKV isn't defined' check if you added hashMapImpl in your
-- local context.
-- Note: if you get 'unexpected "f"', include C.funConstCtx in current context.
hashMapPOD :: String -> String -> DecsQ
hashMapPOD key value = do 
  typedef <- C.verbatim $ "typedef HashMap< " ++ key ++ ", " ++ value ++ " >" ++ hashMapName ++ ";"
  body <- sequence [
      newHashMapKV ^:: [t| IO (Ptr $hashMapType) |]
    , newHashMapKV ^= quoteExp C.exp (hashMapName ++ "* { new " ++ hashMapName ++ "()}")

    , deleteHashMapKV ^:: [t| Ptr $hashMapType -> IO () |]
    , mkFunc1 deleteHashMapKV "ptr" $ \ptrName -> 
        quoteExp C.exp ("void { delete $(" ++ hashMapName ++ "* " ++ show ptrName ++ ")}")
    ]
  createable <- [d| 
      instance Createable (Ptr $hashMapType) where 
        type CreationOptions (Ptr $hashMapType) = ()

        newObject _ = liftIO $(varE $ mkName newHashMapKV)
        deleteObject = liftIO . $(varE $ mkName deleteHashMapKV)
      |]
  hashMapClass <- [d|
      instance HashMap $hashMapType $keyType $valueType where
        hashMapInsert _k _v _m = with _k $ \_k' -> with _v $ \_v' -> 
          $(quoteExp C.exp ("void { $(" ++ hashMapName ++ "* _m)->Insert(" ++ pairKV ++ "(*$(" ++ key ++ "* _k'), *$(" ++ value ++ "* _v'))) }"))
        hashMapLookup _k _m = with _k $ \_k' -> do
          _v <- $(quoteExp C.exp (value ++ "* { $(" ++ hashMapName ++ "* _m)->Contains(*$(" ++ key ++ "* _k')) ? &(*$(" ++ hashMapName ++ "* _m))[*$(" ++ key ++ "* _k')] : NULL }"))
          checkNullPtr' _v peek
        hashMapForeach _m _handler = do 
          let _handler' k v = do 
                k' <- peek k 
                v' <- peek v 
                _handler k' v'
          $(quoteExp C.block ( "void {\n"
              ++ "HashMap<"++key++", "++value++">::Iterator it = $(" ++ hashMapName ++ "* _m)->Begin();\n"
              ++ "while(it != $(" ++ hashMapName ++ "* _m)->End()) {\n"
              ++ "  HashMap<"++key++", "++value++">::KeyValue kv = *it;\n"
              ++ "  $funConst:(void (*_handler')(const "++key++"*, "++value++"*))(&kv.first_, &kv.second_);\n"
              ++ "}"
            ++ "}"
            ))
      |]
  return $ typedef ++ body ++ createable ++ hashMapClass
  where 
  hashMapName = "HashMap" ++ key ++ value
  hashMapType = conT $ mkName hashMapName
  keyType = conT $ mkName key 
  valueType = conT $ mkName value 
  pairKV = "Pair< " ++ key ++ ", " ++ value ++ " >"
  newHashMapKV = "newHashMap" ++ key ++ value
  deleteHashMapKV = "deleteHashMap" ++ key ++ value