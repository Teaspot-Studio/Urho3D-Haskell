{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Graphics.Urho3D.Container.Ptr(
    sharedPtr 
  , sharedPtrImpl 
  ) where

import Graphics.Urho3D.Template
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Foreign

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Createable
import Control.Monad.IO.Class
import qualified Data.Map as Map

-- | Makes public API of SharedPtr for given type
-- Makes following symbols:
-- newSharedTPtr
-- deleteSharedTPtr
-- instance Createable SharedT
--
-- Depends on symbols from @sharedPtrImpl@.
--
-- Note: if you get something like 'SharedT isn't defined' check if you added sharedTPtrCntx in your
-- local context.
sharedPtr :: String -> DecsQ 
sharedPtr tname = do 
  typedef <- C.verbatim $ "typedef SharedPtr<" ++ tname ++ "> " ++ sharedT ++ ";"
  body <- sequence [
      newSharedTPtr ^:: [t| Ptr $tType -> IO $sharedTPtrType |]
    , mkFunc1 newSharedTPtr "ptr" $ \ptrName -> 
        quoteExp C.exp (sharedT ++ "* { new " ++ sharedT ++ "($( "++ tname ++ "* "++show ptrName++")) }")

    , deleteSharedTPtr ^:: [t| $sharedTPtrType -> IO () |]
    , mkFunc1 deleteSharedTPtr "ptr" $ \ptrName -> 
        quoteExp C.exp ("void { delete $(" ++ sharedT ++ "* "++show ptrName++")}")
    ]
  createable <- [d| 
      instance Createable $sharedTType where 
        type CreationOptions $sharedTType = Ptr $tType 

        newObject = liftIO . $(varE $ mkName newSharedTPtr)
        deleteObject = liftIO . $(varE $ mkName deleteSharedTPtr)
      |]
  return $ typedef ++ body ++ createable
  where 
  tType = conT $ mkName tname
  sharedTType = conT $ mkName sharedT 
  sharedTPtrType = conT $ mkName sharedTPtr

  newSharedTPtr = "newShared" ++ tname ++ "Ptr"
  deleteSharedTPtr = "deleteShared" ++ tname ++ "Ptr"
  sharedT = "Shared" ++ tname                                 
  sharedTPtr = sharedT ++ "Ptr"

-- | Makes internal representation of SharedPtr for given type
-- Makes following symbols:
-- data SharedT
-- type SharedTPtr = Ptr SharedT
-- sharedTPtrCntx :: C.Context
sharedPtrImpl :: String -> DecsQ 
sharedPtrImpl tname = do 
  sequence [
      return $ DataD [] (mkName sharedT) [] [] []
    , fmap (TySynD (mkName sharedTPtr) []) [t| Ptr $(return $ ConT $ mkName sharedT) |]

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