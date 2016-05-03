module Graphics.Urho3D.Container.Vector.Internal.Common(
    PODVectorWord8
  , PODVectorWord
  , PODVectorMatrix3x4
  , PODVectorBool
  , PODVectorFloat
  , PODVectorInt
  , VectorPODVectorWord
  , VectorPODVectorMatrix3x4
  , vectorCntx
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data PODVectorWord8
data PODVectorWord
data PODVectorMatrix3x4
data PODVectorBool
data PODVectorFloat
data PODVectorInt
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
    , (C.TypeName "VectorPODVectorWord", [t| VectorPODVectorWord |])
    , (C.TypeName "VectorPODVectorMatrix3x4", [t| VectorPODVectorMatrix3x4 |])
    ]
  } 
