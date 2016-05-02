{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.Variant(
    Variant
  , VariantType
  , ResourceRef(..)
  , ResourceRefList
  , HasObjectType(..)
  , HasObjectName(..)
  , HasObjectNames(..)
  , variantContext
  , variantType
  , VariantStorable(..)
  , getVariantOrError
  , newVariant
  , newVariantMaybe
  , withVariant
  , withVariantMaybe
  , VariantMap
  , HashMapStringHashVariant
  , variantMapGet
  , variantMapGet'
  , variantMapGetDefault
  , VectorVariant
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C
import qualified Data.Text as T 
import Text.RawString.QQ
import Data.Typeable 

import Control.DeepSeq
import Data.Maybe (fromMaybe)
import Data.Monoid
import Foreign 
import Foreign.C.String 
import Graphics.Urho3D.Container.HashMap
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Core.Internal.Variant
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.Rect 
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Monad

C.context (C.cppCtx <> C.funConstCtx <> variantCntx <> variantMapCntx <> stringHashContext <> stringContext <> vector2Context <> vector3Context <> rectContext)
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

-- | Fail with error with cannot get value from variant
getVariantOrError :: forall a m . (VariantStorable a, Typeable a, MonadIO m) => Ptr Variant -> m a 
getVariantOrError ptr = do 
  mval <- getVariant ptr
  case mval of 
    Nothing -> do 
      t <- liftIO $ variantType ptr
      fail $ "Failed to get value from Variant, expected " <> aname <> ", but got " <> show t 
    Just val -> return val 
  where 
    aname = show $ typeRep (Proxy :: Proxy a)

instance VariantStorable Bool where 
  setVariant a ptr = let val = fromBool a
    in liftIO [C.exp| void { *$(Variant* ptr) = $(int val) != 0 } |]
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

C.verbatim [r|
template <class T>
class Traits
{
public:
    struct AlignmentFinder
    {
      char a; 
      T b;
    };

    enum {AlignmentOf = sizeof(AlignmentFinder) - sizeof(T)};
};
|]

instance Storable ResourceRef where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(ResourceRef) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<ResourceRef>::AlignmentOf } |]
  peek ptr = do 
    _resourceRefObjectType <- peek =<< [C.exp| StringHash* { &$(ResourceRef* ptr)->type_ } |]
    _resourceRefObjectName <- peekCString =<< [C.exp| const char* { $(ResourceRef* ptr)->name_.CString() } |]
    return $ ResourceRef {..}
  poke ptr ResourceRef {..} = 
    with _resourceRefObjectType $ \_resourceRefObjectType' -> 
    withCString _resourceRefObjectName $ \_resourceRefObjectName' ->
      [C.block| void { 
      $(ResourceRef* ptr)->type_ = *$(StringHash* _resourceRefObjectType');
      $(ResourceRef* ptr)->name_ = String($(const char * _resourceRefObjectName'));
      } |]

instance Storable ResourceRefList where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(ResourceRefList) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<ResourceRefList>::AlignmentOf } |]
  peek ptr = do 
    _resourceRefListObjectType <- peek =<< [C.exp| StringHash* { &$(ResourceRefList* ptr)->type_ } |]
    _resourceRefListObjectNames <- peekForeignVectorAs =<< [C.exp| StringVector* { &$(ResourceRefList* ptr)->names_ } |]
    return $ ResourceRefList {..}
  poke ptr ResourceRefList {..} = 
    with _resourceRefListObjectType $ \_resourceRefListObjectType' -> 
      withForeignVector () _resourceRefListObjectNames $ \_resourceRefListObjectNames' -> do 
        [C.block| void { 
        $(ResourceRefList* ptr)->type_ = *$(StringHash* _resourceRefListObjectType');
        $(ResourceRefList* ptr)->names_ = StringVector(*$(StringVector* _resourceRefListObjectNames'));
        } |]

C.verbatim "typedef Vector<Variant> VectorVariant;"

instance Createable (Ptr VectorVariant) where 
  type CreationOptions (Ptr VectorVariant) = ()
  newObject _ = liftIO [C.exp| VectorVariant* {new Vector<Variant>() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorVariant* ptr) } |]

instance ReadableVector VectorVariant where 
  type ReadVecElem VectorVariant = Ptr Variant
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorVariant* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ [C.exp| Variant* { &((*$(VectorVariant* ptr))[$(unsigned int i')]) } |]
    where i' = fromIntegral i 

instance WriteableVector VectorVariant where 
  type WriteVecElem VectorVariant = Ptr Variant
  foreignVectorAppend ptr e = liftIO $ [C.exp| void {$(VectorVariant* ptr)->Push(*$(Variant* e)) } |]