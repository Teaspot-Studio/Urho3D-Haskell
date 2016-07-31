{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Quaternion(
    Quaternion(..)
  , HasX(..)
  , HasY(..)
  , HasZ(..)
  , HasW(..)
  , quaternionContext
  , quaternionFromEuler
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.Internal.Quaternion
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Text.RawString.QQ
import System.IO.Unsafe (unsafePerformIO)
import Control.DeepSeq 

C.context (C.cppCtx <> quaternionCntx)
C.include "<Urho3D/Math/Quaternion.h>"
C.using "namespace Urho3D"

quaternionContext :: C.Context 
quaternionContext = quaternionCntx

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

instance Storable Quaternion where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Quaternion) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Quaternion>::AlignmentOf } |]
  peek ptr = do 
    vx <- realToFrac <$> [C.exp| float { $(Quaternion* ptr)->x_ } |]
    vy <- realToFrac <$> [C.exp| float { $(Quaternion* ptr)->y_ } |]
    vz <- realToFrac <$> [C.exp| float { $(Quaternion* ptr)->z_ } |]
    vw <- realToFrac <$> [C.exp| float { $(Quaternion* ptr)->w_ } |]
    return $ Quaternion vx vy vz vw
  poke ptr (Quaternion vx vy vz vw) = [C.block| void { 
    $(Quaternion* ptr)->x_ = $(float vx');
    $(Quaternion* ptr)->y_ = $(float vy');
    $(Quaternion* ptr)->z_ = $(float vz');
    $(Quaternion* ptr)->w_ = $(float vw');
    } |]
    where
    vx' = realToFrac vx 
    vy' = realToFrac vy 
    vz' = realToFrac vz 
    vw' = realToFrac vw

-- | Helper that frees memory after loading
loadQuaternion :: IO (Ptr Quaternion) -> IO Quaternion
loadQuaternion io = do 
  qp <- io
  q <- peek qp 
  q `deepseq` [C.exp| void { delete $(Quaternion* qp) } |]
  return q 

-- | Make quaternion from Euler angles
quaternionFromEuler :: Float -- ^ pitch
  -> Float -- ^ yaw
  -> Float -- ^ roll
  -> Quaternion
quaternionFromEuler ax ay az = unsafePerformIO $ do 
  loadQuaternion $ [C.exp| Quaternion* {
    new Quaternion($(float x'), $(float y'), $(float z'))
  } |]
  where 
    x' = realToFrac ax 
    y' = realToFrac ay 
    z' = realToFrac az

instance Creatable (Ptr Quaternion) where
  type CreationOptions (Ptr Quaternion) = Quaternion

  newObject = liftIO . new
  deleteObject = liftIO . free