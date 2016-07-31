{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Matrix3x4(
    Matrix3x4(..)
  , HasRow1(..)
  , HasRow2(..)
  , HasRow3(..)
  , matrix3x4Context
  , loadMatrix3x4
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.Internal.Matrix3x4
import Graphics.Urho3D.Math.Vector4 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Text.RawString.QQ
import Control.DeepSeq 

C.context (C.cppCtx <> matrix3x4Cntx)
C.include "<Urho3D/Math/Matrix3x4.h>"
C.using "namespace Urho3D"

matrix3x4Context :: C.Context 
matrix3x4Context = matrix3x4Cntx

C.verbatim [r|
template <class T>
class Traits
{
public:
    struct AlignmentFinder
    {
      char a; 
      T b;
    };

    enum {AlignmentOf = sizeof(AlignmentFinder) - sizeof(T)};
};
|]

instance Storable Matrix3x4 where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Matrix3x4) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Matrix3x4>::AlignmentOf } |]
  peek ptr = do 
    m00 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m00_ } |]
    m01 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m01_ } |]
    m02 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m02_ } |]
    m03 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m03_ } |]

    m10 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m10_ } |]
    m11 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m11_ } |]
    m12 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m12_ } |]
    m13 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m13_ } |]

    m20 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m20_ } |]
    m21 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m21_ } |]
    m22 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m22_ } |]
    m23 <- realToFrac <$> [C.exp| float { $(Matrix3x4* ptr)->m23_ } |]

    return $ Matrix3x4 (Vector4 m00 m01 m02 m03) (Vector4 m10 m11 m12 m13) (Vector4 m20 m21 m22 m23)

  poke ptr (Matrix3x4 (Vector4 m00 m01 m02 m03) (Vector4 m10 m11 m12 m13) (Vector4 m20 m21 m22 m23)) = [C.block| void { 
    $(Matrix3x4* ptr)->m00_ = $(float m00');
    $(Matrix3x4* ptr)->m01_ = $(float m01');
    $(Matrix3x4* ptr)->m02_ = $(float m02');
    $(Matrix3x4* ptr)->m03_ = $(float m03');

    $(Matrix3x4* ptr)->m10_ = $(float m10');
    $(Matrix3x4* ptr)->m11_ = $(float m11');
    $(Matrix3x4* ptr)->m12_ = $(float m12');
    $(Matrix3x4* ptr)->m13_ = $(float m13');

    $(Matrix3x4* ptr)->m20_ = $(float m20');
    $(Matrix3x4* ptr)->m21_ = $(float m21');
    $(Matrix3x4* ptr)->m22_ = $(float m22');
    $(Matrix3x4* ptr)->m23_ = $(float m23');
    } |]
    where
    m00' = realToFrac m00 
    m01' = realToFrac m01 
    m02' = realToFrac m02 
    m03' = realToFrac m03 

    m10' = realToFrac m10 
    m11' = realToFrac m11 
    m12' = realToFrac m12 
    m13' = realToFrac m13 

    m20' = realToFrac m20 
    m21' = realToFrac m21 
    m22' = realToFrac m22 
    m23' = realToFrac m23 

-- | Helper that frees memory after loading
loadMatrix3x4 :: IO (Ptr Matrix3x4) -> IO Matrix3x4
loadMatrix3x4 io = do 
  qp <- io
  q <- peek qp 
  q `deepseq` [C.exp| void { delete $(Matrix3x4* qp) } |]
  return q 

instance Creatable (Ptr Matrix3x4) where
  type CreationOptions (Ptr Matrix3x4) = Matrix3x4

  newObject = liftIO . new
  deleteObject = liftIO . free