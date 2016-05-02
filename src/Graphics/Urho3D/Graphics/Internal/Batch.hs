module Graphics.Urho3D.Graphics.Internal.Batch(
    Batch 
  , InstanceData
  , BatchGroup
  , BatchGroupKey
  , BatchQueue
  , ShadowBatchQueue
  , LightBatchQueue
  , batchCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

-- | Queued 3D geometry draw call.
data Batch

-- | Data for one geometry instance.
data InstanceData

-- | Instanced 3D geometry draw call.
data BatchGroup

-- | Instanced draw call grouping key.
data BatchGroupKey

-- | Queue that contains both instanced and non-instanced draw calls.
data BatchQueue

-- | Queue for shadow map draw calls
data ShadowBatchQueue

-- | Queue for light related draw calls.
data LightBatchQueue

batchCntx :: C.Context 
batchCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Batch", [t| Batch |])
    , (C.TypeName "InstanceData", [t| InstanceData |])
    , (C.TypeName "BatchGroup", [t| BatchGroup |])
    , (C.TypeName "BatchGroupKey", [t| BatchGroupKey |])
    , (C.TypeName "BatchQueue", [t| BatchQueue |])
    , (C.TypeName "ShadowBatchQueue", [t| ShadowBatchQueue |])
    , (C.TypeName "LightBatchQueue", [t| LightBatchQueue |])

    ]
  }
