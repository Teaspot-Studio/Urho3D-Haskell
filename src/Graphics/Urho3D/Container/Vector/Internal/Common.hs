module Graphics.Urho3D.Container.Vector.Internal.Common(
    PODVectorWord8
  , PODVectorBillboard
  , PODVectorBool
  , PODVectorFloat
  , PODVectorInt
  , PODVectorMatrix3x4
  , PODVectorVertexElement
  , PODVectorWord
  , VectorPODVectorWord
  , VectorPODVectorMatrix3x4
  , SharedArrayWord8
  , sharedArrayWord8PtrCntx
  , podVectorVertexElementCntx
  , vectorCntx
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector

import qualified Data.Map as Map

data PODVectorWord8
data PODVectorWord
data PODVectorMatrix3x4
data PODVectorBool
data PODVectorFloat
data PODVectorInt
data PODVectorBillboard
data VectorPODVectorWord
data VectorPODVectorMatrix3x4

vectorCntx :: C.Context
vectorCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "PODVectorWord8", [t| PODVectorWord8 |])
    , (C.TypeName "PODVectorWord", [t| PODVectorWord |])
    , (C.TypeName "PODVectorBool", [t| PODVectorBool |])
    , (C.TypeName "PODVectorFloat", [t| PODVectorFloat |])
    , (C.TypeName "PODVectorInt", [t| PODVectorInt |])
    , (C.TypeName "PODVectorMatrix3x4", [t| PODVectorMatrix3x4 |])
    , (C.TypeName "PODVectorBillboard", [t| PODVectorBillboard |])
    , (C.TypeName "VectorPODVectorWord", [t| VectorPODVectorWord |])
    , (C.TypeName "VectorPODVectorMatrix3x4", [t| VectorPODVectorMatrix3x4 |])
    ]
  }

sharedArrayPtrImpl "Word8"
simplePODVectorImpl "VertexElement"
