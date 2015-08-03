{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.Scene.Internal.Node(
    Node
  , nodeCntx
  , sharedNodePtrCntx
  , SharedNode
  , SharedNodePtr
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Node

nodeCntx :: C.Context 
nodeCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Node", [t| Node |])
    ]
  }

sharedPtrImpl "Node"