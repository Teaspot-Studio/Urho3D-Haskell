{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.Node(
    Node
  , nodeContext
  , SharedNode
  , SharedNodePtr
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Scene.Internal.Node
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Control.Monad.IO.Class
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> nodeCntx <> sharedNodePtrCntx <> contextCntx)
C.include "<Urho3D/Scene/Node.h>"
C.using "namespace Urho3D"

nodeContext :: C.Context 
nodeContext = sharedNodePtrCntx <> nodeCntx

newNode :: Ptr Context -> IO (Ptr Node)
newNode ptr = [C.exp| Node* { new Node($(Context* ptr)) } |]

deleteNode :: Ptr Node -> IO ()
deleteNode ptr = [C.exp| void { delete $(Node* ptr) } |]

instance Createable Node where 
  type CreationOptions Node = Ptr Context 

  newObject = liftIO . newNode
  deleteObject = liftIO . deleteNode

sharedPtr "Node"