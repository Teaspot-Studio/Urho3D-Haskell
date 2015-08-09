{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.Context(
    Context
  , contextContext
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.Context
import Graphics.Urho3D.Createable
import Control.Monad.IO.Class
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> contextCntx)
C.include "<Urho3D/Core/Context.h>"
C.using "namespace Urho3D"

contextContext :: C.Context 
contextContext = contextCntx 

newContext :: IO (Ptr Context)
newContext = [C.exp| Context* { new Context() } |]

deleteContext :: Ptr Context -> IO ()
deleteContext ptr = [C.exp| void { delete $(Context* ptr) } |]

instance Createable (Ptr Context) where 
  type CreationOptions (Ptr Context) = ()

  newObject _ = liftIO newContext
  deleteObject = liftIO . deleteContext