{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.Technique(
    Technique
  , SharedTechnique
  , techniqueContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Technique
import Data.Monoid
import Foreign

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx
  <> techniqueCntx
  <> resourceContext
  <> objectContext
  <> sharedTechniquePtrCntx
  <> contextContext
  )

C.include "<Urho3D/Graphics/Technique.h>"
C.using "namespace Urho3D"

techniqueContext :: C.Context
techniqueContext = techniqueCntx
  <> resourceContext
  <> sharedTechniquePtrCntx

deriveParents [''Object, ''Resource] ''Technique

instance ResourceType Technique where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Technique::GetTypeStatic().Value() } |]

instance Creatable (Ptr Technique) where
  type CreationOptions (Ptr Technique) = Ptr Context
  newObject ptr = liftIO [C.exp| Technique* {new Technique($(Context* ptr))} |]
  deleteObject ptr = liftIO [C.exp| void {delete $(Technique* ptr)} |]

sharedPtr "Technique"
