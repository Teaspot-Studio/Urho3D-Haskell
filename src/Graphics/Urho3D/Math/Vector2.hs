{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Vector2(
    Vector2(..)
  , IntVector2(..)
  , HasX(..)
  , HasY(..)
  , vector2Context
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Math.Internal.Vector2
import Data.Monoid
import Foreign 
import Text.RawString.QQ

C.context (C.cppCtx <> vector2Cntx)
C.include "<Urho3D/Math/Vector2.h>"
C.using "namespace Urho3D"

vector2Context :: C.Context 
vector2Context = vector2Cntx

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

instance Storable Vector2 where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Vector2) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Vector2>::AlignmentOf } |]
  peek ptr = do 
    vx <- realToFrac <$> [C.exp| float { $(Vector2* ptr)->x_ } |]
    vy <- realToFrac <$> [C.exp| float { $(Vector2* ptr)->y_ } |]
    return $ Vector2 vx vy
  poke ptr (Vector2 vx vy) = [C.block| void { 
    $(Vector2* ptr)->x_ = $(float vx');
    $(Vector2* ptr)->y_ = $(float vy');
    } |]
    where
    vx' = realToFrac vx 
    vy' = realToFrac vy 

instance Storable IntVector2 where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(IntVector2) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<IntVector2>::AlignmentOf } |]
  peek ptr = do 
    vx <- fromIntegral <$> [C.exp| int { $(IntVector2* ptr)->x_ } |]
    vy <- fromIntegral <$> [C.exp| int { $(IntVector2* ptr)->y_ } |]
    return $ IntVector2 vx vy
  poke ptr (IntVector2 vx vy) = [C.block| void { 
    $(IntVector2* ptr)->x_ = $(int vx');
    $(IntVector2* ptr)->y_ = $(int vy');
    } |]
    where
    vx' = fromIntegral vx 
    vy' = fromIntegral vy 