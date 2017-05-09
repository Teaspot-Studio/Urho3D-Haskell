{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Model(
    Model
  , modelContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Model
import Data.Monoid

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx <> modelCntx <> resourceContext <> objectContext)
C.include "<Urho3D/Graphics/Model.h>"
C.using "namespace Urho3D"

modelContext :: C.Context
modelContext = modelCntx <> resourceContext

deriveParents [''Object, ''Resource] ''Model

instance ResourceType Model where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Model::GetTypeStatic().Value() } |]
