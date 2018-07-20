{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Monad(
  -- | Handling null ptrs
    NullObjectPointerException
  , guardNullPtr
  , checkNullPtr
  , checkNullPtr'
  , checkNullPtrWith
  , wrapNullPtr
  -- | Pointer abstraction
  , Parent(..)
  , Pointer(..)
  -- | Pointer utilities
  , parentPointer
  , mkParentPointer
  -- | Monad utilities
  , whenJust
  , whenM
  , maybeNull
  , module X
  -- | Foreign utils
  , textAsPtrW32
  , textFromPtrW32
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

-- | Null pointer exception with binded location info where the error is thrown
-- TODO: Is attaching location is good idea?
data NullObjectPointerException = NullObjectPointerException
  deriving (Typeable, Show)

instance Exception NullObjectPointerException

-- | Checks that pointer of context isn't equal NULL
-- Will throw NullObjectPointerException
guardNullPtr :: (MonadThrow m, Pointer p a) => p -> m p
guardNullPtr p = checkNullPtr p return

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
  else Just <$> handler ptr

-- | Checks given ptr to be equal null, if then returns given @err@ as error
-- if not null runs handler with this pointer
checkNullPtrWith :: (Exception e, MonadThrow m, Pointer p a) => e -> p -> (p -> m b) -> m b
checkNullPtrWith err ptr handler = if isNull ptr
  then throwM err
  else handler ptr

-- | Replace null with 'Nothing'
wrapNullPtr :: Pointer p a => p -> Maybe p
wrapNullPtr ptr = if isNull ptr then Nothing else Just ptr

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
