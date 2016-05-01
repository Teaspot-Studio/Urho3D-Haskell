module Graphics.Urho3D.Math.Internal.Vector3(
    Vector3(..)
  , PODVectorVector3
  , VectorPODVectorVector3
  , vector3Cntx
  , HasX(..)
  , HasY(..)
  , HasZ(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map as Map
import Graphics.Urho3D.Math.Internal.Vector2
import Control.Lens 
import GHC.Generics
import Control.DeepSeq

import Data.Vector.Unboxed
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import Control.Monad ( liftM )

data Vector3 = Vector3 {
  _vector3X :: {-# UNPACK #-} !Float 
, _vector3Y :: {-# UNPACK #-} !Float 
, _vector3Z :: {-# UNPACK #-} !Float   
} deriving (Eq, Ord, Show, Generic)

instance NFData Vector3
makeFields ''Vector3

data PODVectorVector3
data VectorPODVectorVector3

vector3Cntx :: C.Context 
vector3Cntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Vector3", [t| Vector3 |])
    , (C.TypeName "PODVectorVector3", [t| PODVectorVector3 |])
    , (C.TypeName "VectorPODVectorVector3", [t| VectorPODVectorVector3 |])
    ]
  }

newtype instance MVector s Vector3 = MV_Vector3 (MVector s (Float, Float, Float))
newtype instance Vector    Vector3 = V_Vector3  (Vector    (Float, Float, Float))

instance Unbox Vector3

instance M.MVector MVector Vector3 where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Vector3 v) = M.basicLength v
  basicUnsafeSlice i n (MV_Vector3 v) = MV_Vector3 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Vector3 v1) (MV_Vector3 v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Vector3 `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Vector3 v) = M.basicInitialize v
  basicUnsafeReplicate n (Vector3 _x _y _z) = MV_Vector3 `liftM` M.basicUnsafeReplicate n (_x,_y,_z)
  basicUnsafeRead (MV_Vector3 v) i = (\(_x,_y,_z)->Vector3 _x _y _z) `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Vector3 v) i (Vector3 _x _y _z) = M.basicUnsafeWrite v i (_x,_y,_z)
  basicClear (MV_Vector3 v) = M.basicClear v
  basicSet (MV_Vector3 v) (Vector3 _x _y _z) = M.basicSet v (_x,_y,_z)
  basicUnsafeCopy (MV_Vector3 v1) (MV_Vector3 v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Vector3 v1) (MV_Vector3 v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Vector3 v) n = MV_Vector3 `liftM` M.basicUnsafeGrow v n

instance G.Vector Vector Vector3 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Vector3 v) = V_Vector3 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Vector3 v) = MV_Vector3 `liftM` G.basicUnsafeThaw v
  basicLength (V_Vector3 v) = G.basicLength v
  basicUnsafeSlice i n (V_Vector3 v) = V_Vector3 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Vector3 v) i
                = (\(_x,_y,_z)->Vector3 _x _y _z) `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Vector3 mv) (V_Vector3 v)
                = G.basicUnsafeCopy mv v
  elemseq _ (Vector3 _x _y _z) w = G.elemseq (undefined :: Vector a) _x
                              $ G.elemseq (undefined :: Vector a) _y
                              $ G.elemseq (undefined :: Vector a) _z w
