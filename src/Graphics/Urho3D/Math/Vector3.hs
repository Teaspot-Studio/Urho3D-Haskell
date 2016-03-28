{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Vector3(
    Vector3(..)
  , HasX(..)
  , HasY(..)
  , HasZ(..)
  , vector3Context
  , vec3Left
  , vec3Right
  , vec3Up
  , vec3Down
  , vec3Forward
  , vec3Back
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Control.Lens 
import Data.Monoid
import Foreign 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.Defs
import Graphics.Urho3D.Math.Internal.Vector3
import Graphics.Urho3D.Monad
import Text.RawString.QQ

C.context (C.cppCtx <> vector3Cntx)
C.include "<Urho3D/Math/Vector3.h>"
C.using "namespace Urho3D"

vector3Context :: C.Context 
vector3Context = vector3Cntx

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

instance Storable Vector3 where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Vector3) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Vector3>::AlignmentOf } |]
  peek ptr = do 
    vx <- realToFrac <$> [C.exp| float { $(Vector3* ptr)->x_ } |]
    vy <- realToFrac <$> [C.exp| float { $(Vector3* ptr)->y_ } |]
    vz <- realToFrac <$> [C.exp| float { $(Vector3* ptr)->z_ } |]
    return $ Vector3 vx vy vz
  poke ptr (Vector3 vx vy vz) = [C.block| void { 
    $(Vector3* ptr)->x_ = $(float vx');
    $(Vector3* ptr)->y_ = $(float vy');
    $(Vector3* ptr)->z_ = $(float vz');
    } |]
    where
    vx' = realToFrac vx 
    vy' = realToFrac vy 
    vz' = realToFrac vz

instance Num Vector3 where 
  a + b = Vector3 (a^.x + b^.x) (a^.y + b^.y) (a^.z + b^.z)
  a - b = Vector3 (a^.x - b^.x) (a^.y - b^.y) (a^.z - b^.z)
  a * b = Vector3 (a^.x * b^.x) (a^.y * b^.y) (a^.z * b^.z)
  abs a = Vector3 (abs $ a^.x) (abs $ a^.y) (abs $ a^.z)
  signum a = Vector3 (signum $ a^.x) (signum $ a^.y) (signum $ a^.z)
  fromInteger i = Vector3 (fromIntegral i) (fromIntegral i) (fromIntegral i)

instance Fractional Vector3 where 
  a / b = Vector3 (a^.x / b^.x) (a^.y / b^.y) (a^.z / b^.z)
  fromRational v = Vector3 (fromRational v) (fromRational v) (fromRational v)

instance Createable (Ptr Vector3) where
  type CreationOptions (Ptr Vector3) = Vector3

  newObject = liftIO . new
  deleteObject = liftIO . free

instance UrhoRandom Vector3 where 
  random = Vector3 <$> random <*> random <*> random
  randomUp maxv = Vector3 
    <$> randomUp (maxv^.x) 
    <*> randomUp (maxv^.y)
    <*> randomUp (maxv^.z)
  randomRange minv maxv = Vector3 
    <$> randomRange (minv^.x) (maxv^.x) 
    <*> randomRange (minv^.y) (maxv^.y)
    <*> randomRange (minv^.z) (maxv^.z)

-- | (-1,0,0) vector.
vec3Left :: Vector3
vec3Left = Vector3 (-1) 0 0

-- | (1,0,0) vector.
vec3Right :: Vector3 
vec3Right = Vector3 1 0 0

-- | (0,1,0) vector.
vec3Up :: Vector3
vec3Up = Vector3 0 1 0

-- | (0,-1,0) vector.
vec3Down :: Vector3 
vec3Down = Vector3 0 (-1) 0

-- | (0,0,1) vector.
vec3Forward :: Vector3
vec3Forward = Vector3 0 0 1

-- | (0,0,-1) vector.
vec3Back :: Vector3
vec3Back = Vector3 0 0 (-1)