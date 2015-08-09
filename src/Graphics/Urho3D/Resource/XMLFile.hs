{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Resource.XMLFile(
    XMLFile 
  , xmlFileContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Resource.Internal.XMLFile
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Control.Monad.IO.Class
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> xmlFileCntx <> contextContext)
C.include "<Urho3D/Resource/XMLFile.h>"
C.using "namespace Urho3D"

xmlFileContext :: C.Context 
xmlFileContext = xmlFileCntx

newXMLFile :: Ptr Context -> IO (Ptr XMLFile)
newXMLFile ptr = [C.exp| XMLFile* { new XMLFile( $(Context* ptr) ) } |]

deleteXMLFile :: Ptr XMLFile -> IO ()
deleteXMLFile ptr = [C.exp| void { delete $(XMLFile* ptr) } |]

instance Createable XMLFile where 
  type CreationOptions XMLFile = Ptr Context 

  newObject = liftIO . newXMLFile
  deleteObject = liftIO . deleteXMLFile
