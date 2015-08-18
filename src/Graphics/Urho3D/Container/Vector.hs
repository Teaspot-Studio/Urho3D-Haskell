{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Graphics.Urho3D.Container.Vector(
    podVectorPtr
  , podVectorPtrImpl
  ) where

import qualified Data.Map as Map
import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Foreign
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Template
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Makes PODVector<T*> for given type
-- Makes following symbols:
-- instance Createable PODVectorTPtr
-- instance ReadableVector PODVectorTPtr
-- instance WriteableVector PODVectorTPtr
podVectorPtr :: String -> DecsQ
podVectorPtr elemName = do 
  typedef <- C.verbatim $ "typedef " ++ vectorCpp ++ " " ++ vectorT ++ ";"
  createable <- [d|
    instance Createable (Ptr $vectorType) where 
      type CreationOptions (Ptr $vectorType) = ()

      newObject _ = liftIO $(quoteExp C.exp $ vectorT++"* {new "++vectorCpp++"()}")
      deleteObject _ptr = liftIO $ $(quoteExp C.exp $ "void { delete $("++vectorT++"* _ptr)}")
    |]
  readable <- [d|
    instance ReadableVector $vectorType where 
      type ReadVecElem $vectorType = Ptr $elemType

      foreignVectorLength _ptr = liftIO $ fromIntegral <$> $(quoteExp C.exp $ "unsigned int {$("++vectorT++"* _ptr)->Size()}")
      foreignVectorElement _ptr _i = liftIO $(quoteExp C.exp $ elemName ++ "* {(*$("++vectorT++"* _ptr))[$(int _i')]}")
        where _i' = fromIntegral _i
    |]
  writeable <- [d|
    instance WriteableVector $vectorType where 
      type WriteVecElem $vectorType = Ptr $elemType 

      foreignVectorAppend _ptr _elem = liftIO $(quoteExp C.exp $ "void {$("++vectorT++"* _ptr)->Push($("++elemName++"* _elem))}")
    |]
  return $ typedef ++ createable ++ readable ++ writeable
  where 
    vectorType = conT $ mkName vectorT
    elemType = conT $ mkName elemName 
    vectorT = "PODVector" ++ elemName ++ "Ptr"
    vectorCpp = "PODVector<" ++ elemName ++ "*>"

-- | Makes internal representation of PODVector<T*> for given type
-- Makes following symbols:
-- data PODVectorTPtr
-- podVectorTPtrCntx :: C.Context
podVectorPtrImpl :: String -> DecsQ
podVectorPtrImpl elemName = sequence [
    return $ DataD [] (mkName vectorT) [] [] []
  , podVectorTPtrCntx ^:: [t| C.Context |]
  , podVectorTPtrCntx ^= [e| mempty {
        C.ctxTypesTable = Map.fromList [
          (C.TypeName $cTType, $cntxTType)
        ]
    } |]
  ]
  where 
    vectorT = "PODVector" ++ elemName ++ "Ptr"
    podVectorTPtrCntx = "podVector" ++ elemName ++ "PtrCntx"
    cTType = return $ LitE $ StringL vectorT
    cntxTType = [e| return $ ConT $ mkName $cTType |]