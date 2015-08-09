{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.StringHash(
    StringHash 
  , stringHashContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Math.Internal.StringHash
import Graphics.Urho3D.Createable
import Control.Monad.IO.Class
import Data.Monoid
import Foreign 
import Foreign.C.String 

C.context (C.cppCtx <> stringHashCntx)
C.include "<Urho3D/Math/StringHash.h>"
C.using "namespace Urho3D"

stringHashContext :: C.Context 
stringHashContext = stringHashCntx

newStringHash :: String -> IO (Ptr StringHash)
newStringHash str = withCString str $ \cstr -> do
  [C.exp| StringHash* { new StringHash( $(const char* cstr) ) } |]

deleteStringHash :: Ptr StringHash -> IO ()
deleteStringHash ptr = [C.exp| void { delete $(StringHash* ptr) } |]

instance Createable StringHash where 
  type CreationOptions StringHash = String

  newObject = liftIO . newStringHash
  deleteObject = liftIO . deleteStringHash