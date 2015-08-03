{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Engine.Application(
    Application
  , applicationCntx
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Engine.Internal.Application
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Control.Monad.IO.Class
import Data.Monoid
--import Data.StateVar
import Foreign 

C.context (C.cppCtx <> applicationCntx <> contextCntx)
C.include "<Urho3D/Engine/Engine.h>"
C.include "<Urho3D/Engine/Application.h>"
C.using "namespace Urho3D"

newApplication :: Ptr Context -> IO (Ptr Application)
newApplication ptr = [C.exp| Application* { new Application($(Context* ptr)) } |]

deleteApplication :: Ptr Application -> IO ()
deleteApplication ptr = [C.exp| void { delete $(Application* ptr) } |]

instance Createable Application where 
  type CreationOptions Application = Ptr Context 

  newObject = liftIO . newApplication
  deleteObject = liftIO . deleteApplication

--setStartupParameter :: Ptr Application -> String -> 