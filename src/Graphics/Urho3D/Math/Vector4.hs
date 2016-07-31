{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Vector4(
    Vector4(..)
  , HasX(..)
  , HasY(..)
  , HasZ(..)
  , HasW(..)
  , vector4Context
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Control.Lens 
import Data.Monoid
import Foreign 
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.Defs
import Graphics.Urho3D.Math.Internal.Vector4
import Graphics.Urho3D.Monad
import Text.RawString.QQ

C.context (C.cppCtx <> vector4Cntx)
C.include "<Urho3D/Math/Vector4.h>"
C.using "namespace Urho3D"

vector4Context :: C.Context 
vector4Context = vector4Cntx

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

instance Storable Vector4 where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Vector4) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Vector4>::AlignmentOf } |]
  peek ptr = do 
    vx <- realToFrac <$> [C.exp| float { $(Vector4* ptr)->x_ } |]
    vy <- realToFrac <$> [C.exp| float { $(Vector4* ptr)->y_ } |]
    vz <- realToFrac <$> [C.exp| float { $(Vector4* ptr)->z_ } |]
    vw <- realToFrac <$> [C.exp| float { $(Vector4* ptr)->w_ } |]
    return $ Vector4 vx vy vz vw
  poke ptr (Vector4 vx vy vz vw) = [C.block| void { 
    $(Vector4* ptr)->x_ = $(float vx');
    $(Vector4* ptr)->y_ = $(float vy');
    $(Vector4* ptr)->z_ = $(float vz');
    $(Vector4* ptr)->w_ = $(float vw');
    } |]
    where
    vx' = realToFrac vx 
    vy' = realToFrac vy 
    vz' = realToFrac vz
    vw' = realToFrac vw

instance Num Vector4 where 
  a + b = Vector4 (a^.x + b^.x) (a^.y + b^.y) (a^.z + b^.z) (a^.w + b^.w)
  a - b = Vector4 (a^.x - b^.x) (a^.y - b^.y) (a^.z - b^.z) (a^.w - b^.w)
  a * b = Vector4 (a^.x * b^.x) (a^.y * b^.y) (a^.z * b^.z) (a^.w * b^.w)
  abs a = Vector4 (abs $ a^.x) (abs $ a^.y) (abs $ a^.z) (abs $ a^.w)
  signum a = Vector4 (signum $ a^.x) (signum $ a^.y) (signum $ a^.z) (signum $ a^.w)
  fromInteger i = Vector4 (fromIntegral i) (fromIntegral i) (fromIntegral i) (fromIntegral i)

instance Fractional Vector4 where 
  a / b = Vector4 (a^.x / b^.x) (a^.y / b^.y) (a^.z / b^.z) (a^.w / b^.w)
  fromRational v = Vector4 (fromRational v) (fromRational v) (fromRational v) (fromRational v)

instance Creatable (Ptr Vector4) where
  type CreationOptions (Ptr Vector4) = Vector4

  newObject = liftIO . new
  deleteObject = liftIO . free

instance UrhoRandom Vector4 where 
  random = Vector4 <$> random <*> random <*> random <*> random
  randomUp maxv = Vector4 
    <$> randomUp (maxv^.x) 
    <*> randomUp (maxv^.y)
    <*> randomUp (maxv^.z)
    <*> randomUp (maxv^.w)
  randomRange minv maxv = Vector4 
    <$> randomRange (minv^.x) (maxv^.x) 
    <*> randomRange (minv^.y) (maxv^.y)
    <*> randomRange (minv^.z) (maxv^.z)
    <*> randomRange (minv^.w) (maxv^.w)