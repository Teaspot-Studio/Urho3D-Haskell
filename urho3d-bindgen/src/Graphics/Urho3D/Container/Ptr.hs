module Graphics.Urho3D.Container.Ptr(
    SharedPtr
  , WeakPtr
  , newSharedObject
  , weakPtr
  , peekSharedPtr
  , withSharedPtr
  , peekWeakPtr
  , withWeakPtr
  -- | Utilities for implementing shared pointers
  , SharedPointerFinalizer(..)
  , WeakPointerFinalizer(..)
  , sharedPtr
  , sharedPtrImpl
  , sharedWeakPtr
  , sharedWeakPtrImpl
  ) where

import Graphics.Urho3D.Template
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Foreign
import Foreign.Concurrent as FC

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import Graphics.Urho3D.Multithread

-- | Shared pointer template class with intrusive reference counting.
--
-- This is high-level wrapper that is used in API. Low-level wrappers are generated for each type via Template Haskell.
data SharedPtr a = SharedPtr {
  unSharedPtr :: ForeignPtr a
, sharedPtrOrig :: Ptr () -- ^ Pointer to original SharedPtr<a>
}

-- | Weak pointer template class with intrusive reference counting. Does not keep the object pointed to alive.
--
-- This is high-level wrapper that is used in API. Low-level wrappers are generated for each type via Template Haskell.
data WeakPtr a = WeakPtr {
  unWeakPtr :: ForeignPtr a
, weakPtrOrig :: Ptr () -- ^ Pointer to original WeakPtr<a>
}

-- | Note that 'pointer' function is not safe for SharedPtr as finalizer can run earlier than expected.
instance SharedPointerFinalizer p a => Pointer (SharedPtr a) a where
  pointer (SharedPtr fp _) = unsafeForeignPtrToPtr fp
  isNull (SharedPtr fp _) = unsafePerformIO . withForeignPtr fp $ \ptr -> return (ptr == nullPtr)
  makePointer ptr = uncurry SharedPtr . unsafePerformIO $ do
    (pptr, fptr) <- makeSharedForeignPointer ptr
    return (fptr, castPtr pptr)

-- | Note that 'pointer' function is not safe for SharedPtr as finalizer can run earlier than expected.
instance WeakPointerFinalizer p a => Pointer (WeakPtr a) a where
  pointer (WeakPtr fp _) = unsafeForeignPtrToPtr fp
  isNull (WeakPtr fp _) = unsafePerformIO . withForeignPtr fp $ \ptr -> return (ptr == nullPtr)
  makePointer ptr = uncurry WeakPtr . unsafePerformIO $ do
    (pptr, fptr) <- makeWeakForeignPointer ptr
    return (fptr, castPtr pptr)

class SharedPointerFinalizer pointer element | pointer -> element, element -> pointer where
  -- | Add automatic finalizer to the pointer and create SharedPtr<element>. Used in 'makePointer' of 'SharedPtr'.
  --
  -- When haskell code looses returned foreign pointer, linked cpp-sided shared pointer is deleted.
  -- Note that finalizer must delete linked SharedPtr (or WeakPtr) in main thread.
  makeSharedForeignPointer :: Ptr element -> IO (Ptr pointer, ForeignPtr element)
  -- | Add automatic finalizer to the pointer. The function uses existing shared (cpp sided) pointer
  --
  -- When haskell code looses returned foreign pointer, linked cpp-sided shared pointer is deleted.
  -- Note that finalizer must delete linked SharedPtr (or WeakPtr) in main thread.
  wrapSharedPointer :: Ptr pointer -> IO (ForeignPtr element)

class WeakPointerFinalizer pointer element | pointer -> element, element -> pointer where
  -- | Add automatic finalizer to the pointer and create WeakPtr<element>. Used in 'makePointer' of 'WeakPtr'.
  --
  -- When haskell code looses returned foreign pointer, linked cpp-sided shared pointer is deleted.
  -- Note that finalizer must delete linked SharedPtr (or WeakPtr) in main thread.
  makeWeakForeignPointer :: Ptr element -> IO (Ptr pointer, ForeignPtr element)
  -- | Add automatic finalizer to the pointer. The function uses existing shared (cpp sided) pointer
  --
  -- When haskell code looses returned foreign pointer, linked cpp-sided shared pointer is deleted.
  -- Note that finalizer must delete linked SharedPtr (or WeakPtr) in main thread.
  wrapWeakPointer :: Ptr pointer -> IO (ForeignPtr element)

-- | Main peeking function that is used in API
peekSharedPtr :: (SharedPointerFinalizer p a, MonadIO m) => Ptr p -> m (SharedPtr a)
peekSharedPtr ptr = liftIO $ do
  fptr <- wrapSharedPointer ptr
  return $ SharedPtr fptr (castPtr ptr)

-- | Main poking function that is used in API
withSharedPtr :: (SharedPointerFinalizer p a, MonadIO m) => SharedPtr a -> (Ptr p -> m b) -> m b
withSharedPtr SharedPtr{..} io = io (castPtr sharedPtrOrig)

-- | Main peeking function that is used in API
peekWeakPtr :: (WeakPointerFinalizer p a, MonadIO m) => Ptr p -> m (WeakPtr a)
peekWeakPtr ptr = liftIO $ do
  fptr <- wrapWeakPointer ptr
  return $ WeakPtr fptr (castPtr ptr)

-- | Main poking function that is used in API
withWeakPtr :: (WeakPointerFinalizer p a, MonadIO m) => WeakPtr a -> (Ptr p -> m b) -> m b
withWeakPtr WeakPtr{..} io = io (castPtr weakPtrOrig)

-- | Creates new object and wraps it to shared pointer. Pointer is garbage collected. The created object
-- will be destroyed as soon as there is no shared pointers pointed to it.
newSharedObject :: (SharedPointerFinalizer p a, Creatable (Ptr a), MonadIO m) => CreationOptions (Ptr a) -> m (SharedPtr a)
newSharedObject opts = liftIO $ do
  ptr <- newObject opts
  (pptr, fptr) <- makeSharedForeignPointer ptr
  return $ SharedPtr fptr (castPtr pptr)

-- | Get weak reference to object from shared pointer
weakPtr :: (WeakPointerFinalizer pw a, SharedPointerFinalizer ps a) => SharedPtr a -> WeakPtr a
weakPtr = makePointer . pointer

-- ==============================================================================================
-- Implementation details
-- ==============================================================================================

-- | Makes public API of SharedPtr for given type
-- Makes following symbols:
-- deleteSharedTPtr
-- instance SharedPointerFinalizer SharedT T
--
-- Depends on symbols from @sharedPtrImpl@.
--
-- Note: if you get something like 'SharedT isn't defined' check if you added sharedTPtrCntx in your
-- local context.
sharedPtr :: String -> DecsQ
sharedPtr tname = do
#ifndef SHARED_DEBUG
  typedef <- C.verbatim $ "typedef SharedPtr<" ++ tname ++ "> " ++ sharedT ++ ";"
#else
  typedef <- C.verbatim $ "#include <iostream>\ntypedef SharedPtr<" ++ tname ++ "> " ++ sharedT ++ ";"
#endif
  deleter <- sequence [
      deleteSharedTPtr ^:: [t| Ptr $sharedTType -> IO () |]
    , mkFunc1 deleteSharedTPtr "ptr" $ \ptrName -> [e|runInMainThread $ do
        $(let inlinePtr = "$(" ++ sharedT ++ "* "++show ptrName++")"
#ifndef SHARED_DEBUG
          in quoteExp C.exp ("void { if ("++inlinePtr++") { delete "++inlinePtr++"; } }") )
#else
          in quoteExp C.exp ("void { std::cout << \"finalizing shared "++tname++" \" << "++inlinePtr++"->Refs() << std::endl; if ("++inlinePtr++") { delete "++inlinePtr++"; } }") )
#endif
        |]
    ]

  finalizerInst <- [d|
      instance SharedPointerFinalizer $sharedTType $tType where
        makeSharedForeignPointer ptr = do
          pptr <- $(quoteExp C.exp (sharedT ++ "* { new " ++ sharedT ++ "($( "++ tname ++ "* ptr)) }"))
          fptr <- FC.newForeignPtr ptr $ $(varE $ mkName deleteSharedTPtr) pptr
          return (pptr, fptr)
        wrapSharedPointer pptr = do
          ptr <- $(quoteExp C.exp (tname ++ "* { $("++sharedT++"* pptr)->Get() }"))
          fptr <- FC.newForeignPtr ptr $ $(varE $ mkName deleteSharedTPtr) pptr
          return fptr
      |]

  return $ typedef ++ deleter ++ finalizerInst

  where
  tType = conT $ mkName tname
  sharedTType = conT $ mkName sharedT

  deleteSharedTPtr = "deleteShared" ++ tname ++ "Ptr"
  sharedT = "Shared" ++ tname

-- | Makes public API of SharedWeakPtr for given type
-- Makes following symbols:
-- deleteWeakTPtr
--
-- instance WeakPointerFinalizer WeakT T
--
-- Depends on symbols from @sharedWeakPtrImpl@.
--
-- Note: if you get something like 'WeakT isn't defined' check if you added sharedWeakTPtrCntx in your
-- local context.
sharedWeakPtr :: String -> DecsQ
sharedWeakPtr tname = do
#ifndef SHARED_DEBUG
  typedef <- C.verbatim $ "typedef WeakPtr<" ++ tname ++ "> " ++ weakT ++ ";"
#else
  typedef <- C.verbatim $ "#include <iostream>\ntypedef WeakPtr<" ++ tname ++ "> " ++ weakT ++ ";"
#endif
  deleter <- sequence [
      deleteWeakTPtr ^:: [t| Ptr $weakTType -> IO () |]
    , mkFunc1 deleteWeakTPtr "ptr" $ \ptrName -> [e|runInMainThread $ do
        $(let inlinePtr = "$(" ++ weakT ++ "* "++show ptrName++")"
#ifndef SHARED_DEBUG
          in quoteExp C.exp ("void { if ("++inlinePtr++") { delete "++inlinePtr++"; } }") )
#else
          in quoteExp C.exp ("void { std::cout << \"finalizing weak "++tname++"\" << std::endl; if ("++inlinePtr++") { delete "++inlinePtr++"; } }") )
#endif
        |]
    ]

  finalizerInst <- [d|
      instance WeakPointerFinalizer $weakTType $tType where
        makeWeakForeignPointer ptr = do
          pptr <- $(quoteExp C.exp (weakT ++ "* { new " ++ weakT ++ "($( "++ tname ++ "* ptr)) }"))
          fptr <- FC.newForeignPtr ptr $ $(varE $ mkName deleteWeakTPtr) pptr
          return (pptr, fptr)
        wrapWeakPointer pptr = do
          ptr <- $(quoteExp C.exp (tname ++ "* { $("++weakT++"* pptr)->Get() }"))
          fptr <- FC.newForeignPtr ptr $ $(varE $ mkName deleteWeakTPtr) pptr
          return fptr
      |]

  return $ typedef ++ deleter ++ finalizerInst

  where
  tType = conT $ mkName tname
  weakTType = conT $ mkName weakT

  deleteWeakTPtr = "deleteWeak" ++ tname ++ "Ptr"
  weakT = "Weak" ++ tname

-- | Makes internal representation of SharedPtr for given type
-- Makes following symbols:
-- data SharedT
-- sharedTPtrCntx :: C.Context
sharedPtrImpl :: String -> DecsQ
sharedPtrImpl tname = do
  sequence [
      return $ DataD [] (mkName sharedT) [] Nothing [] []
    , sharedTPtrCntx ^:: [t| C.Context |]
    , sharedTPtrCntx ^= [e| mempty {
        C.ctxTypesTable = Map.fromList [
          (C.TypeName $cTType, $cntxTType)
        ]
      } |]
    ]
  where
  sharedT = "Shared" ++ tname
  sharedTPtrCntx = "shared" ++ tname  ++ "PtrCntx"
  cTType = return $ LitE $ StringL sharedT
  cntxTType = [e| return $ ConT $ mkName $cTType |]

-- | Makes internal representation of SharedWeakPtr for given type
-- Makes following symbols:
-- data WeakT
-- weakTPtrCntx :: C.Context
sharedWeakPtrImpl :: String -> DecsQ
sharedWeakPtrImpl tname = do
  sequence [
      return $ DataD [] (mkName sharedT) [] Nothing [] []
    , sharedTPtrCntx ^:: [t| C.Context |]
    , sharedTPtrCntx ^= [e| mempty {
        C.ctxTypesTable = Map.fromList [
          (C.TypeName $cTType, $cntxTType)
        ]
      } |]
    ]
  where
  sharedT = "Weak" ++ tname
  sharedTPtrCntx = "weak" ++ tname  ++ "PtrCntx"
  cTType = return $ LitE $ StringL sharedT
  cntxTType = [e| return $ ConT $ mkName $cTType |]
