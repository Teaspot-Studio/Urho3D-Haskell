module Graphics.Urho3D.Container.Vector(
    podVectorPtr
  , podVectorPtr'
  , podVectorPtrImpl
  , simpleVector
  , simpleVector'
  , simpleVectorImpl
  ) where

import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Foreign
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Template
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Makes PODVector<T*> for given type
-- Makes following symbols:
-- instance Creatable PODVectorTPtr
-- instance ReadableVector PODVectorTPtr
-- instance WriteableVector PODVectorTPtr
podVectorPtr :: String -> DecsQ
podVectorPtr elemName = podVectorPtr' elemName elemName

-- | Same as 'podVectorPtr', but you can provide different names for C-side and Haskell-side.
podVectorPtr' :: String -> String -> DecsQ
podVectorPtr' cElemName elemName = do
  typedef <- C.verbatim $ "typedef " ++ vectorCpp ++ " " ++ vectorT ++ ";"
  createable <- [d|
    instance Creatable (Ptr $vectorType) where
      type CreationOptions (Ptr $vectorType) = ()

      newObject _ = liftIO $(quoteExp C.exp $ vectorT++"* {new "++vectorCpp++"()}")
      deleteObject _ptr = liftIO $ $(quoteExp C.exp $ "void { delete $("++vectorT++"* _ptr)}")
    |]
  readable <- [d|
    instance ReadableVector $vectorType where
      type ReadVecElem $vectorType = Ptr $elemType

      foreignVectorLength _ptr = liftIO $ fromIntegral <$> $(quoteExp C.exp $ "unsigned int {$("++vectorT++"* _ptr)->Size()}")
      foreignVectorElement _ptr _i = liftIO $(quoteExp C.exp $ cElemName ++ "* {(*$("++vectorT++"* _ptr))[$(int _i')]}")
        where _i' = fromIntegral _i
    |]
  writeable <- [d|
    instance WriteableVector $vectorType where
      type WriteVecElem $vectorType = Ptr $elemType

      foreignVectorAppend _ptr _elem = liftIO $(quoteExp C.exp $ "void {$("++vectorT++"* _ptr)->Push($("++cElemName++"* _elem))}")
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
    return $ DataD [] (mkName vectorT) [] Nothing [] []
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


-- | Makes Vector<T> for given type. You need Storable instance for T.
-- Makes following symbols:
-- instance Creatable VectorT
-- instance ReadableVector VectorT
-- instance WriteableVector VectorT
simpleVector :: String -> DecsQ
simpleVector elemName = simpleVector' elemName elemName

-- | Same as 'simpleVector', but you can provide different names for C-side and Haskell-side.
simpleVector' :: String -> String -> DecsQ
simpleVector' cElemName elemName = do
  typedef <- C.verbatim $ "typedef " ++ vectorCpp ++ " " ++ vectorT ++ ";"
  createable <- [d|
    instance Creatable (Ptr $vectorType) where
      type CreationOptions (Ptr $vectorType) = ()

      newObject _ = liftIO $(quoteExp C.exp $ vectorT++"* {new "++vectorCpp++"()}")
      deleteObject _ptr = liftIO $ $(quoteExp C.exp $ "void { delete $("++vectorT++"* _ptr)}")
    |]
  readable <- [d|
    instance ReadableVector $vectorType where
      type ReadVecElem $vectorType = $elemType

      foreignVectorLength _ptr = liftIO $ fromIntegral <$> $(quoteExp C.exp $ "unsigned int {$("++vectorT++"* _ptr)->Size()}")
      foreignVectorElement _ptr _i = liftIO $ peek =<< $(quoteExp C.exp $ cElemName ++ "* {&((*$("++vectorT++"* _ptr))[$(int _i')])}")
        where _i' = fromIntegral _i
    |]
  writeable <- [d|
    instance WriteableVector $vectorType where
      type WriteVecElem $vectorType = $elemType

      foreignVectorAppend _ptr _elem = liftIO $ with _elem $ \_elem' ->  $(quoteExp C.exp $ "void {$("++vectorT++"* _ptr)->Push(*$("++cElemName++"* _elem'))}")
    |]
  return $ typedef ++ createable ++ readable ++ writeable
  where
    vectorType = conT $ mkName vectorT
    elemType = conT $ mkName elemName
    vectorT = "Vector" ++ elemName ++ ""
    vectorCpp = "Vector<" ++ elemName ++ ">"

-- | Makes internal representation of Vector<T> for given type
-- Makes following symbols:
-- data VectorT
-- vectorTCntx :: C.Context
simpleVectorImpl :: String -> DecsQ
simpleVectorImpl elemName = sequence [
    return $ DataD [] (mkName vectorT) [] Nothing [] []
  , vectorTPtrCntx ^:: [t| C.Context |]
  , vectorTPtrCntx ^= [e| mempty {
        C.ctxTypesTable = Map.fromList [
          (C.TypeName $cTType, $cntxTType)
        ]
    } |]
  ]
  where
    vectorT = "Vector" ++ elemName
    vectorTPtrCntx = "vector" ++ elemName ++ "Cntx"
    cTType = return $ LitE $ StringL vectorT
    cntxTType = [e| return $ ConT $ mkName $cTType |]
