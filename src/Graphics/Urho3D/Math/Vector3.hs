{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Vector3(
    Vector3(..)
  , HasX(..)
  , HasY(..)
  , HasZ(..)
  , vector3Context
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Math.Internal.Vector3
import Data.Monoid
import Foreign 
import Text.RawString.QQ
import Control.Lens 

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