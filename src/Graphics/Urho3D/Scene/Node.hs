{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Node(
    Node
  , nodeContext
  , SharedNode
  , SharedNodePtr
  , NodeComponent(..)
  , nodeGetComponent
  , nodeGetRotation
  , nodeSetRotation
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Scene.Internal.Node
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Quaternion
import Graphics.Urho3D.Monad
import Data.Monoid
import Data.Proxy
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> nodeCntx <> sharedNodePtrCntx <> contextContext <> stringHashContext <> componentContext <> quaternionContext)
C.include "<Urho3D/Scene/Node.h>"
C.include "<Urho3D/Scene/Component.h>"
C.using "namespace Urho3D" 

nodeContext :: C.Context 
nodeContext = sharedNodePtrCntx <> nodeCntx <> stringHashContext <> componentContext

newNode :: Ptr Context -> IO (Ptr Node)
newNode ptr = [C.exp| Node* { new Node($(Context* ptr)) } |]

deleteNode :: Ptr Node -> IO ()
deleteNode ptr = [C.exp| void { delete $(Node* ptr) } |]

instance Createable (Ptr Node) where 
  type CreationOptions (Ptr Node) = Ptr Context 

  newObject = liftIO . newNode
  deleteObject = liftIO . deleteNode

sharedPtr "Node" 

class Parent Component a => NodeComponent a where 
  nodeComponentType :: Proxy a -> Ptr StringHash 

instance NodeComponent Component where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = Component::GetTypeStatic();
    return &h;
  } |]

nodeGetComponent :: forall a p c m . (Parent Node a, Pointer p a, MonadIO m, NodeComponent c) 
  => p -> m (Maybe (Ptr c))
nodeGetComponent p = liftIO $ do 
  let ptr = parentPointer p 
      ct = nodeComponentType (Proxy :: Proxy c)
  cp <- [C.exp| Component* { $(Node* ptr)->GetComponent(*$(StringHash* ct)) } |]
  join <$> checkNullPtr' cp (return . castToChild)

-- | Returns node rotation in quaternion
nodeGetRotation :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> m Quaternion
nodeGetRotation p = liftIO $ do 
  let ptr = parentPointer p 
  peek =<< [C.exp| const Quaternion* { &$(Node* ptr)->GetRotation() } |]
  
-- | Sets node rotation with quaternion
nodeSetRotation :: (Parent Node a, Pointer p a, MonadIO m) => p -- ^ Node pointer or child
  -> Quaternion -- ^ Rotation
  -> m ()
nodeSetRotation p q = liftIO $ with q $ \q' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Node* ptr)->SetRotation(*$(Quaternion* q'))} |]