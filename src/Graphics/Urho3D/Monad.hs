{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Monad(
    Urho
  , evalUrho
  -- | Handling null ptrs
  , NullObjectPointerException
  , guardNullPtr
  , checkNullPtr
  , checkNullPtr'
  , checkNullPtrWith
  , liftContext
  , Parent(..)
  , Pointer(..)
  , whenJust
  , whenM
  , maybeNull
  , module X
  , parentPointer
  , mkParentPointer
  -- | Foreign utils
  , textAsPtrW32
  , textFromPtrW32
  , BitSet(..)
  ) where

import qualified Data.Text as T 

import Foreign
import Foreign.C.Types
import Control.Monad
import Control.Monad.Catch as X
import Control.Monad.Reader as X
import Control.DeepSeq 

import Data.ByteString.Unsafe (unsafeUseAsCString, unsafePackCString)
import Data.Maybe 
import Data.Text.Encoding (encodeUtf32LE, decodeUtf32LE)
import Data.Typeable

-- | Describes monad with context @s@
newtype Urho s a = Urho { unUrho :: ReaderT (Ptr s) IO a}
  deriving (Functor, Applicative, Monad, MonadReader (Ptr s), MonadIO, MonadThrow, MonadCatch, MonadMask) 

-- | Performs computation stored in @Urho@ monad
evalUrho :: Ptr s -> Urho s a -> IO a
evalUrho s m = runReaderT (unUrho m) s

-- | Null pointer exception with binded location info where the error is thrown
-- TODO: Is attaching location is good idea?
data NullObjectPointerException = NullObjectPointerException 
  deriving (Typeable, Show)

instance Exception NullObjectPointerException

-- | Checks that pointer of context isn't equal NULL
-- Will throw NullObjectPointerException
guardNullPtr :: Urho s a -> Urho s a
guardNullPtr f = do 
  ptr <- ask
  if ptr == nullPtr then throwM NullObjectPointerException else return ()
  f 

-- | Checks given ptr to be equal null, if then throws @NullObjectPointerException@
-- if not null runs handler with this pointer
checkNullPtr :: (MonadThrow m, Pointer p a) => p -> (p -> m b) -> m b
checkNullPtr = checkNullPtrWith NullObjectPointerException

-- | Checks given ptr to be equal null, if then returns Nothing
-- if not null runs handler with this pointer
-- Version without custom monad
checkNullPtr' :: (Monad m, Pointer p a) => p -> (p -> m b) -> m (Maybe b)
checkNullPtr' ptr handler = if isNull ptr 
  then return Nothing
  else fmap Just $ handler ptr

-- | Checks given ptr to be equal null, if then returns given @err@ as error
-- if not null runs handler with this pointer
checkNullPtrWith :: (Exception e, MonadThrow m, Pointer p a) => e -> p -> (p -> m b) -> m b
checkNullPtrWith err ptr handler = if isNull ptr
  then throwM err
  else handler ptr

-- | Transforms context of action (e.x. lift Application to sub manager context)
liftContext :: Ptr s -> Urho s a -> Urho s' a 
liftContext s (Urho m) = Urho $ withReaderT (const s) m

-- | Relation between classes, where a is parent for b
class Parent parent child where
  castToParent :: Ptr child -> Ptr parent 
  castToChild :: Ptr parent -> Maybe (Ptr child)

instance Parent a a where 
  castToParent = id 
  castToChild = Just 

{-
instance (Parent a b, Parent b c) => Parent a c where 
  castToParent = (castToParent :: Ptr b -> Ptr a) . (castToParent :: Ptr c -> Ptr b)
  castToChild a = do 
    b <- castToChild a :: Maybe (Ptr b)
    c <- castToChild b :: Maybe (Ptr c)
    return c 
-}

-- | Relation between types, where a is pointer for b 
class Pointer pointer ptype | pointer -> ptype where 
  pointer :: pointer -> Ptr ptype
  isNull :: pointer -> Bool 
  makePointer :: Ptr ptype -> pointer 

instance Pointer (Ptr a) a where 
  pointer = id 
  isNull p = p == nullPtr
  makePointer = id 

-- | If me have pointer to child, we have pointer to parent
mkParentPointer :: (Parent parent child, Pointer p1 child, Pointer p2 parent) => p1 -> p2
mkParentPointer = makePointer . parentPointer

-- | If me have pointer to child, we have pointer to parent
parentPointer :: (Parent parent child, Pointer p1 child) => p1 -> Ptr parent
parentPointer = castToParent . pointer

-- | Like @when@ but operates with Maybe
whenJust :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
whenJust Nothing _ = return Nothing
whenJust (Just a) f = Just <$> f a

-- | Lifted version of @when@
whenM :: Monad m => m Bool -> m () -> m ()
whenM cond f = do 
  v <- cond 
  when v f

-- | Converts text to UTF32 and then passes a copy to handler
textAsPtrW32 :: T.Text -> (Ptr CWchar -> IO a) -> IO a
textAsPtrW32 t = unsafeUseAsCString (encodeUtf32LE $ t `T.snoc` '\0') . (. castPtr)

-- | Converts text from UTF32 
textFromPtrW32 :: Ptr CWchar -> IO T.Text 
textFromPtrW32 ptr = decodeUtf32LE <$> unsafePackCString (castPtr ptr)

-- | Same as @Prelude.maybe@, but operates with pointer
maybeNull :: Pointer p a => b -> (p -> b) -> p -> b 
maybeNull b f ptr
  | isNull ptr = b 
  | otherwise = f ptr

instance NFData (Ptr a) where 
  rnf = (`seq` ())

-- | Converting enums to bitset and vice-versa
class BitSet a where 
  toByteBitset :: [a] -> Word8
  fromByteBitset :: Word8 -> [a]

-- | Assumes that enum instance uses only powers of two
instance Enum a => BitSet a where 
  toByteBitset = sum . fmap (fromIntegral . fromEnum)
  fromByteBitset w = catMaybes $ extractBit <$> [0 .. 7]
    where
    extractBit i = if testBit w i
      then Just . toEnum $ 2^i 
      else Nothing