{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Batch(
    Batch
  , InstanceData
  , BatchGroup
  , BatchGroupKey
  , BatchQueue
  , ShadowBatchQueue
  , LightBatchQueue
  , batchContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Batch
import Data.Monoid

C.context (C.cppCtx <> batchCntx )
C.include "<Urho3D/Graphics/Batch.h>"
C.using "namespace Urho3D"

batchContext :: C.Context 
batchContext = batchCntx 