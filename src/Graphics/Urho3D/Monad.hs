{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
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
  , module X
  ) where

import Foreign
import Control.Monad
import Control.Monad.Catch as X
import Control.Monad.Reader as X

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

-- | Checks given ptr to be equal null, if then throws @ODBErrorNullObjectPointer@
-- if not null runs handler with this pointer
checkNullPtr :: MonadThrow m => Ptr a -> (Ptr a -> m b) -> m b
checkNullPtr = checkNullPtrWith NullObjectPointerException

-- | Checks given ptr to be equal null, if then returns Nothing
-- if not null runs handler with this pointer
-- Version without custom monad
checkNullPtr' :: Monad m => Ptr a -> (Ptr a -> m b) -> m (Maybe b)
checkNullPtr' ptr handler = if ptr == nullPtr 
  then return Nothing
  else fmap Just $ handler ptr

-- | Checks given ptr to be equal null, if then returns given @err@ as error
-- if not null runs handler with this pointer
checkNullPtrWith :: (Exception e, MonadThrow m) => e -> Ptr a -> (Ptr a -> m b) -> m b
checkNullPtrWith err ptr handler = if ptr == nullPtr 
  then throwM err
  else handler ptr

-- | Transforms context of action (e.x. lift Application to sub manager context)
liftContext :: Ptr s -> Urho s a -> Urho s' a 
liftContext s (Urho m) = Urho $ withReaderT (const s) m

-- | Relation between classes, where a is parent for b
class Parent a b where
  castToParent :: Ptr b -> Ptr a 
  castToChild :: Ptr a -> Maybe (Ptr b)

-- | Relation between types, where a is pointer for b 
class Pointer a b | a -> b where 
  pointer :: a -> Ptr b

-- | Like @when@ but operates with Maybe
whenJust :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
whenJust Nothing _ = return Nothing
whenJust (Just a) f = Just <$> f a