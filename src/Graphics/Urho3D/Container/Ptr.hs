module Graphics.Urho3D.Container.Ptr(
    sharedPtr 
  , sharedPtrImpl
  , sharedWeakPtr
  , sharedWeakPtrImpl
  , SharedPointer(..)
  , AbstractType
  ) where

import Graphics.Urho3D.Template
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Foreign
import Foreign.Concurrent as FC 
import Data.Monoid 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Multithread

-- | Common operations with shared pointers. 
class (Pointer pointer element, Createable (Ptr element)) => SharedPointer pointer element | pointer -> element where 
  -- | Creates new object and wraps it to shared pointer. Pointer is garbage collected. The created object
  -- will be destroyed as soon as there is no shared pointers pointed to it.
  newSharedObject :: CreationOptions (Ptr element) -> IO pointer

-- | Create instance of the class if you don't want producing
-- errors about missing 'Createable' instance when defining shared ptr.
class AbstractType a 

-- | Makes public API of SharedPtr for given type
-- Makes following symbols:
-- newSharedTPtr
-- deleteSharedTPtr
-- instance Createable SharedT
-- instance Pointer SharedTPtr T 
-- 
-- wrapSharedTPtr :: Ptr SharedT -> IO SharedTPtr
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

  wrappPointer <- sequence [
      wrapSharedTPtr ^:: [t| Ptr $sharedTType -> IO $sharedTPtrType |]
    , mkFunc1 wrapSharedTPtr "ptr" $ \_ -> [e| do
        fptr <- FC.newForeignPtr $(varE $ mkName "ptr") $ $(varE $ mkName deleteSharedTPtr) $(varE $ mkName "ptr")
        return $ $(conE $ mkName sharedTPtr) fptr
      |]
    ]

  allocator <- sequence [
      newSharedTPtr ^:: [t| Ptr $tType -> IO $sharedTPtrType |]
    , mkFunc1 newSharedTPtr "ptr" $ \ptrName -> [e| do
        sptr <- $(quoteExp C.exp (sharedT ++ "* { new " ++ sharedT ++ "($( "++ tname ++ "* "++show ptrName++")) }")) 
        $(varE $ mkName wrapSharedTPtr) sptr
        |]
    ]

  createable <- [d| 
    instance Createable $sharedTPtrType where 
      type CreationOptions $sharedTPtrType = Ptr $tType 

      newObject = liftIO . $(varE $ mkName newSharedTPtr)
      deleteObject = liftIO . finalizeForeignPtr . $(varE $ mkName unSharedTPtr)
    |]

  pointerCast <- sequence [
      pointerCastF ^:: [t| $sharedTPtrType -> Ptr $tType|]
    , mkFunc1Con pointerCastF sharedTPtr "ptr" $ \ptrName -> [e| unsafePerformIO $ withForeignPtr $(varE ptrName) $ \_ptr ->
        $(quoteExp C.exp $ tname ++ "* {"++"$("++sharedT++"* _ptr)->Get()}") |]
    ]
  pointerInst <- [d|
    instance Pointer $sharedTPtrType $tType where 
      pointer = $(varE $ mkName pointerCastF)
      isNull ptr = pointer ptr == nullPtr
      makePointer = unsafePerformIO . $(varE $ mkName newSharedTPtr)
    |]

  sharedPointerIns <- do 
    ptrTType <- [t|Ptr $tType|]
    isDefined <- ''Createable `isInstance` [ptrTType]
    if not isDefined then do 
      isAbstract <- ''AbstractType `isInstance` [ConT $ mkName tname]
      unless isAbstract $ 
        reportWarning $ "No instance Createable for " 
          <> pprint ptrTType 
          <> ". Add AbstractType instance to supress the warning."
      return []
    else [d|
      instance SharedPointer $sharedTPtrType $tType where 
        newSharedObject opts = newObject opts >>= $(varE $ mkName newSharedTPtr)
      |]

  return $ typedef ++ deleter ++ wrappPointer ++ allocator ++ createable ++ pointerCast ++ pointerInst ++ sharedPointerIns

  where 
  tType = conT $ mkName tname
  sharedTType = conT $ mkName sharedT
  sharedTPtrType = conT $ mkName sharedTPtr

  newSharedTPtr = "newShared" ++ tname ++ "Ptr"
  deleteSharedTPtr = "deleteShared" ++ tname ++ "Ptr"
  sharedT = "Shared" ++ tname                                 
  sharedTPtr = sharedT ++ "Ptr"
  pointerCastF = "pointer" ++ sharedT
  unSharedTPtr = "unShared" ++ tname ++ "Ptr"
  wrapSharedTPtr = "wrap" ++ sharedTPtr

-- | Makes public API of SharedWeakPtr for given type
-- Makes following symbols:
-- newSharedWeakTPtr
-- deleteSharedWeakTPtr
-- instance Createable SharedWeakT
-- instance Pointer SharedWeakTPtr T 
-- 
-- wrapSharedWeakTPtr :: Ptr SharedWeakT -> IO SharedWeakTPtr
-- 
-- Depends on symbols from @sharedWeakPtrImpl@.
--
-- Note: if you get something like 'SharedWeakT isn't defined' check if you added sharedWeakTPtrCntx in your
-- local context.
sharedWeakPtr :: String -> DecsQ 
sharedWeakPtr tname = do 
#ifndef SHARED_DEBUG
  typedef <- C.verbatim $ "typedef WeakPtr<" ++ tname ++ "> " ++ sharedT ++ ";"
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
          in quoteExp C.exp ("void { std::cout << \"finalizing weak "++tname++"\" << std::endl; if ("++inlinePtr++") { delete "++inlinePtr++"; } }") )
#endif
        |]
    ]

  wrappPointer <- sequence [
      wrapSharedTPtr ^:: [t| Ptr $sharedTType -> IO $sharedTPtrType |]
    , mkFunc1 wrapSharedTPtr "ptr" $ \_ -> [e| do
        fptr <- FC.newForeignPtr $(varE $ mkName "ptr") $ $(varE $ mkName deleteSharedTPtr) $(varE $ mkName "ptr")
        return $ $(conE $ mkName sharedTPtr) fptr
      |]
    ]

  allocator <- sequence [
      newSharedTPtr ^:: [t| Ptr $tType -> IO $sharedTPtrType |]
    , mkFunc1 newSharedTPtr "ptr" $ \ptrName -> [e| do
        sptr <- $(quoteExp C.exp (sharedT ++ "* { new " ++ sharedT ++ "($( "++ tname ++ "* "++show ptrName++")) }")) 
        $(varE $ mkName wrapSharedTPtr) sptr
        |]
    ]

  createable <- [d| 
    instance Createable $sharedTPtrType where 
      type CreationOptions $sharedTPtrType = Ptr $tType 

      newObject = liftIO . $(varE $ mkName newSharedTPtr)
      deleteObject = liftIO . finalizeForeignPtr . $(varE $ mkName unSharedTPtr)
    |]

  pointerCast <- sequence [
      pointerCastF ^:: [t| $sharedTPtrType -> Ptr $tType|]
    , mkFunc1Con pointerCastF sharedTPtr "ptr" $ \ptrName -> [e| unsafePerformIO $ withForeignPtr $(varE ptrName) $ \_ptr ->
        $(quoteExp C.exp $ tname ++ "* {"++"$("++sharedT++"* _ptr)->Get()}") |]
    ]
  pointerInst <- [d|
    instance Pointer $sharedTPtrType $tType where 
      pointer = $(varE $ mkName pointerCastF)
      isNull ptr = pointer ptr == nullPtr
      makePointer = unsafePerformIO . $(varE $ mkName newSharedTPtr)
    |]

  sharedPointerIns <- do 
    ptrTType <- [t|Ptr $tType|]
    isDefined <- ''Createable `isInstance` [ptrTType]
    if not isDefined then do 
      isAbstract <- ''AbstractType `isInstance` [ConT $ mkName tname]
      unless isAbstract $ 
        reportWarning $ "No instance Createable for " 
          <> pprint ptrTType 
          <> ". Add AbstractType instance to supress the warning."
      return []
    else [d|
      instance SharedPointer $sharedTPtrType $tType where 
        newSharedObject opts = newObject opts >>= $(varE $ mkName newSharedTPtr)
      |]

  return $ typedef ++ deleter ++ wrappPointer ++ allocator ++ createable ++ pointerCast ++ pointerInst ++ sharedPointerIns

  where 
  tType = conT $ mkName tname
  sharedTType = conT $ mkName sharedT
  sharedTPtrType = conT $ mkName sharedTPtr

  newSharedTPtr = "newSharedWeak" ++ tname ++ "Ptr"
  deleteSharedTPtr = "deleteSharedWeak" ++ tname ++ "Ptr"
  sharedT = "SharedWeak" ++ tname                                 
  sharedTPtr = sharedT ++ "Ptr"
  pointerCastF = "pointer" ++ sharedT
  unSharedTPtr = "unSharedWeak" ++ tname ++ "Ptr"
  wrapSharedTPtr = "wrap" ++ sharedTPtr

-- | Makes internal representation of SharedPtr for given type
-- Makes following symbols:
-- data SharedT
-- newtype SharedTPtr = SharedTPtr { unSharedTPtr :: ForeignPtr SharedT }
-- sharedTPtrCntx :: C.Context
sharedPtrImpl :: String -> DecsQ 
sharedPtrImpl tname = do 
  sequence [
      return $ DataD [] (mkName sharedT) [] [] []
    , mkNewType sharedTPtr [t| ForeignPtr $(return $ ConT $ mkName sharedT) |]
    , sharedTPtrCntx ^:: [t| C.Context |]
    , sharedTPtrCntx ^= [e| mempty { 
        C.ctxTypesTable = Map.fromList [
          (C.TypeName $cTType, $cntxTType)
        ]
      } |]
    ]
  where
  sharedT = "Shared" ++ tname 
  sharedTPtr = sharedT ++ "Ptr"
  sharedTPtrCntx = "shared" ++ tname  ++ "PtrCntx"
  cTType = return $ LitE $ StringL sharedT
  cntxTType = [e| return $ ConT $ mkName $cTType |]

-- | Makes internal representation of SharedWeakPtr for given type
-- Makes following symbols:
-- data SharedWeakT
-- newtype SharedWeakTPtr = SharedWeakTPtr { unSharedWeakTPtr :: ForeignPtr SharedWeakT }
-- sharedWeakTPtrCntx :: C.Context
sharedWeakPtrImpl :: String -> DecsQ 
sharedWeakPtrImpl tname = do 
  sequence [
      return $ DataD [] (mkName sharedT) [] [] []
    , mkNewType sharedTPtr [t| ForeignPtr $(return $ ConT $ mkName sharedT) |]
    , sharedTPtrCntx ^:: [t| C.Context |]
    , sharedTPtrCntx ^= [e| mempty { 
        C.ctxTypesTable = Map.fromList [
          (C.TypeName $cTType, $cntxTType)
        ]
      } |]
    ]
  where
  sharedT = "SharedWeak" ++ tname 
  sharedTPtr = sharedT ++ "Ptr"
  sharedTPtrCntx = "sharedWeak" ++ tname  ++ "PtrCntx"
  cTType = return $ LitE $ StringL sharedT
  cntxTType = [e| return $ ConT $ mkName $cTType |]