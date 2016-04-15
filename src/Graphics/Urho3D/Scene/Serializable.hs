{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Serializable(
    Serializable
  , serializableContext
  , SharedSerializable
  , SharedSerializablePtr
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Scene.Internal.Serializable
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Data.Monoid
import Foreign 

import Graphics.Urho3D.Core.Object

C.context (C.cppCtx <> serializableCntx <> sharedSerializablePtrCntx <> contextContext <> stringHashContext <> objectContext)
C.include "<Urho3D/Scene/Serializable.h>"
C.using "namespace Urho3D" 

serializableContext :: C.Context 
serializableContext = sharedSerializablePtrCntx <> serializableCntx <> stringHashContext

newSerializable :: Ptr Context -> IO (Ptr Serializable)
newSerializable ptr = [C.exp| Serializable* { new Serializable($(Context* ptr)) } |]

deleteSerializable :: Ptr Serializable -> IO ()
deleteSerializable ptr = [C.exp| void { delete $(Serializable* ptr) } |]

instance Createable (Ptr Serializable) where 
  type CreationOptions (Ptr Serializable) = Ptr Context 

  newObject = liftIO . newSerializable
  deleteObject = liftIO . deleteSerializable

sharedPtr "Serializable" 

deriveParent ''Object ''Serializable