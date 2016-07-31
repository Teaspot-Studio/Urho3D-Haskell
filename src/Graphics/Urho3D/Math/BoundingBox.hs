{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.BoundingBox(
    BoundingBox(..)
  , HasMinVector(..)
  , HasMaxVector(..)
  , boundingBoxContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Control.Lens 
import Data.Monoid
import Foreign 
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.Defs
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Math.Internal.BoundingBox
import Graphics.Urho3D.Monad
import Text.RawString.QQ

C.context (C.cppCtx <> boundingBoxCntx <> vector3Context)
C.include "<Urho3D/Math/BoundingBox.h>"
C.using "namespace Urho3D"

boundingBoxContext :: C.Context 
boundingBoxContext = boundingBoxCntx

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

instance Storable BoundingBox where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(BoundingBox) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<BoundingBox>::AlignmentOf } |]
  peek ptr = do 
    minv <- peek =<< [C.exp| Vector3* { &$(BoundingBox* ptr)->min_ } |]
    maxv <- peek =<< [C.exp| Vector3* { &$(BoundingBox* ptr)->max_ } |]
    return $ BoundingBox minv maxv
  poke ptr (BoundingBox minv maxv) = with minv $ \minv' -> with maxv $ \maxv' ->[C.block| void { 
    $(BoundingBox* ptr)->min_ = *$(Vector3* minv');
    $(BoundingBox* ptr)->max_ = *$(Vector3* maxv');
    } |]

instance Num BoundingBox where 
  a + b = BoundingBox (a^.minVector + b^.minVector) (a^.maxVector + b^.maxVector)
  a - b = BoundingBox (a^.minVector - b^.minVector) (a^.maxVector - b^.maxVector)
  a * b = BoundingBox (a^.minVector * b^.minVector) (a^.maxVector * b^.maxVector)
  abs a = BoundingBox (abs $ a^.minVector) (abs $ a^.maxVector)
  signum a = BoundingBox (signum $ a^.minVector) (signum $ a^.maxVector)
  fromInteger i = BoundingBox (fromIntegral i) (fromIntegral i)

instance Fractional BoundingBox where 
  a / b = BoundingBox (a^.minVector / b^.minVector) (a^.maxVector / b^.maxVector)
  fromRational v = BoundingBox (fromRational v) (fromRational v)

instance Creatable (Ptr BoundingBox) where
  type CreationOptions (Ptr BoundingBox) = BoundingBox

  newObject = liftIO . new
  deleteObject = liftIO . free

instance UrhoRandom BoundingBox where 
  random = BoundingBox <$> random <*> random
  randomUp maxv = BoundingBox 
    <$> randomUp (maxv^.minVector) 
    <*> randomUp (maxv^.maxVector)
  randomRange minv maxv = BoundingBox 
    <$> randomRange (minv^.minVector) (maxv^.minVector) 
    <*> randomRange (minv^.maxVector) (maxv^.maxVector)