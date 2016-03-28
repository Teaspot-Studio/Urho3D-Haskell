{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Resource.XMLElement(
    XMLElement 
  , xmlElementContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Resource.Internal.XMLElement
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign

C.context (C.cppCtx <> xmlElementCntx)
C.include "<Urho3D/Resource/XMLElement.h>"
C.using "namespace Urho3D"

xmlElementContext :: C.Context 
xmlElementContext = xmlElementCntx

instance Createable (Ptr XMLElement) where 
  type CreationOptions (Ptr XMLElement) = ()

  newObject _ = liftIO [C.exp| XMLElement* { new XMLElement() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(XMLElement* ptr) } |]
