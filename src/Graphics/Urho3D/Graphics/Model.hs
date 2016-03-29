{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Model(
    Model
  , modelContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Model
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx <> modelCntx <> resourceContext)
C.include "<Urho3D/Graphics/Model.h>"
C.using "namespace Urho3D"

modelContext :: C.Context 
modelContext = modelCntx <> resourceContext

instance Parent Resource Model where 
  castToParent ptr = [C.pure| Resource* { (Resource*)$(Model* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Model* { (Model*)$(Resource* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance ResourceType Model where 
  resourceType _ = unsafePerformIO $ [C.block| StringHash* { 
    static StringHash h = Model::GetTypeStatic(); 
    return &h; 
    } |]