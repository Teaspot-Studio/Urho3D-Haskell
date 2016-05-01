{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Matrix4(
    Matrix4(..)
  , HasRow1(..)
  , HasRow2(..)
  , HasRow3(..)
  , HasRow4(..)
  , matrix4Context
  , loadMatrix4
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.Internal.Matrix4
import Graphics.Urho3D.Math.Vector4 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Text.RawString.QQ
import Control.DeepSeq 

C.context (C.cppCtx <> matrix4Cntx)
C.include "<Urho3D/Math/Matrix4.h>"
C.using "namespace Urho3D"

matrix4Context :: C.Context 
matrix4Context = matrix4Cntx

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

instance Storable Matrix4 where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Matrix4) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Matrix4>::AlignmentOf } |]
  peek ptr = do 
    m00 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m00_ } |]
    m01 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m01_ } |]
    m02 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m02_ } |]
    m03 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m03_ } |]

    m10 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m10_ } |]
    m11 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m11_ } |]
    m12 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m12_ } |]
    m13 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m13_ } |]

    m20 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m20_ } |]
    m21 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m21_ } |]
    m22 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m22_ } |]
    m23 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m23_ } |]

    m30 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m30_ } |]
    m31 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m31_ } |]
    m32 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m32_ } |]
    m33 <- realToFrac <$> [C.exp| float { $(Matrix4* ptr)->m33_ } |]

    return $ Matrix4 (Vector4 m00 m01 m02 m03) (Vector4 m10 m11 m12 m13) (Vector4 m20 m21 m22 m23) (Vector4 m30 m31 m32 m33)

  poke ptr (Matrix4 (Vector4 m00 m01 m02 m03) (Vector4 m10 m11 m12 m13) (Vector4 m20 m21 m22 m23) (Vector4 m30 m31 m32 m33)) = [C.block| void { 
    $(Matrix4* ptr)->m00_ = $(float m00');
    $(Matrix4* ptr)->m01_ = $(float m01');
    $(Matrix4* ptr)->m02_ = $(float m02');
    $(Matrix4* ptr)->m03_ = $(float m03');

    $(Matrix4* ptr)->m10_ = $(float m10');
    $(Matrix4* ptr)->m11_ = $(float m11');
    $(Matrix4* ptr)->m12_ = $(float m12');
    $(Matrix4* ptr)->m13_ = $(float m13');

    $(Matrix4* ptr)->m20_ = $(float m20');
    $(Matrix4* ptr)->m21_ = $(float m21');
    $(Matrix4* ptr)->m22_ = $(float m22');
    $(Matrix4* ptr)->m23_ = $(float m23');

    $(Matrix4* ptr)->m30_ = $(float m30');
    $(Matrix4* ptr)->m31_ = $(float m31');
    $(Matrix4* ptr)->m32_ = $(float m32');
    $(Matrix4* ptr)->m33_ = $(float m33');
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

    m30' = realToFrac m30 
    m31' = realToFrac m31 
    m32' = realToFrac m32 
    m33' = realToFrac m33 

-- | Helper that frees memory after loading
loadMatrix4 :: IO (Ptr Matrix4) -> IO Matrix4
loadMatrix4 io = do 
  qp <- io
  q <- peek qp 
  q `deepseq` [C.exp| void { delete $(Matrix4* qp) } |]
  return q 

instance Createable (Ptr Matrix4) where
  type CreationOptions (Ptr Matrix4) = Matrix4

  newObject = liftIO . new
  deleteObject = liftIO . free