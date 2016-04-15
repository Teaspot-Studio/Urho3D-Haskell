{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Model(
    Model
  , modelContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Model
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> modelCntx <> resourceContext <> objectContext)
C.include "<Urho3D/Graphics/Model.h>"
C.using "namespace Urho3D"

modelContext :: C.Context 
modelContext = modelCntx <> resourceContext

deriveParents [''Object, ''Resource] ''Model

instance ResourceType Model where 
  resourceType _ = unsafePerformIO $ [C.block| StringHash* { 
    static StringHash h = Model::GetTypeStatic(); 
    return &h; 
    } |]