module Graphics.Urho3D.Math.Defs(
    urhoPi
  , halfPi
  , minInt
  , maxInt
  , minUnsigned
  , maxUnsigned
  , epsylon
  , largeEpsilon
  , minNearClip
  , maxFov
  , largeValue
  , infinity
  , degToRad
  , degToRad2
  , radToDeg
  , Intersection(..)
  , equals
  , lerp
  , Clamp(..)
  , smoothStep
  , isPowerOfTwo
  , nextPowerOfTwo
  , countSetBits
  , sdbmHash
  , UrhoRandom(..)
  , floatToHalf
  , halfToFloat
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Monad
import Data.Word
import Foreign

C.context C.cppCtx
C.include "<Urho3D/Math/MathDefs.h>"
C.using "namespace Urho3D"

urhoPi :: Float
urhoPi = 3.14159265358979323846264338327950288

halfPi :: Float
halfPi = urhoPi * 0.5

minInt :: Int
minInt = 0x80000000

maxInt :: Int
maxInt = 0x7fffffff

minUnsigned :: Int
minUnsigned = 0x00000000

maxUnsigned :: Int
maxUnsigned = 0xffffffff

epsylon :: Float
epsylon = 0.000001

largeEpsilon :: Float
largeEpsilon = 0.00005

minNearClip :: Float
minNearClip = 0.01

maxFov :: Float
maxFov = 160

largeValue :: Float
largeValue = 100000000

infinity :: Float
infinity = realToFrac [C.pure| float { (float)HUGE_VAL } |]

degToRad :: Float
degToRad = urhoPi / 180

degToRad2 :: Float
degToRad2 = urhoPi / 360

radToDeg :: Float
radToDeg = 1 / degToRad

-- | Intersection test result
data Intersection =
    IntersectOutside
  | Intersects
  | IntersectInside
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Check whether two floating point values are equal within accuracy.
equals :: Float -> Float -> Bool
equals lhs rhs = toBool [C.pure| int {(int)Equals($(float lhs'), $(float rhs'))} |]
  where
    lhs' = realToFrac lhs
    rhs' = realToFrac rhs

-- | Linear interpolation between two float values.
lerp :: Float -> Float -> Float -> Float
lerp lhs rhs t = realToFrac [C.pure| float {Lerp($(float lhs'), $(float rhs'), $(float t'))} |]
  where
    lhs' = realToFrac lhs
    rhs' = realToFrac rhs
    t' = realToFrac t

class Clamp a where
  -- | Clamp a value to a range.
  clamp :: a -> a -> a -> a

instance Clamp Float where
  clamp value minv maxv = realToFrac [C.pure| float {Clamp($(float value'), $(float min'), $(float max'))} |]
    where
      value' = realToFrac value
      min' = realToFrac minv
      max' = realToFrac maxv

instance Clamp Int where
  clamp value minv maxv = fromIntegral [C.pure| int {Clamp($(int value'), $(int min'), $(int max'))} |]
    where
      value' = fromIntegral value
      min' = fromIntegral minv
      max' = fromIntegral maxv

-- | Smoothly damp between values.
smoothStep :: Float -> Float -> Float -> Float
smoothStep lhs rhs t = realToFrac [C.pure| float {SmoothStep($(float lhs'), $(float rhs'), $(float t'))} |]
  where
    lhs' = realToFrac lhs
    rhs' = realToFrac rhs
    t' = realToFrac t

-- | Check whether an unsigned integer is a power of two.
isPowerOfTwo :: Int -> Bool
isPowerOfTwo value = toBool [C.pure| int {(int)IsPowerOfTwo($(unsigned int value'))} |]
  where
    value' = fromIntegral value

-- | Round up to next power of two.
nextPowerOfTwo :: Int -> Int
nextPowerOfTwo value = fromIntegral [C.pure| unsigned int {NextPowerOfTwo($(unsigned int value'))} |]
  where
    value' = fromIntegral value

-- | Count the number of set bits in a mask.
countSetBits :: Int -> Int
countSetBits value = fromIntegral [C.pure| unsigned int {CountSetBits($(unsigned int value'))} |]
  where
    value' = fromIntegral value

-- | Update a hash with the given 8-bit value using the SDBM algorithm.
sdbmHash :: Int -> Word8 -> Int
sdbmHash hash w = fromIntegral [C.pure| unsigned int {SDBMHash($(unsigned int hash'), $(unsigned char w'))} |]
  where
    hash' = fromIntegral hash
    w' = fromIntegral w

-- | Urho3D random generation utilities
class UrhoRandom a where
  -- | Return a random float between 0.0 (inclusive) and 1.0 (exclusive.) for floating instances
  -- Return a random integer between 0 and 1 inclusive for integral instances
  random :: MonadIO m => m a

  -- | Return a random float between 0.0 and range, inclusive from both ends for floating instances
  -- Return a random integer between 0 and range - 1 for integral instances
  randomUp :: MonadIO m => a -> m a

  -- | Return a random float between min and max, inclusive from both ends for floating instances
  -- Return a random integer between min and max - 1 for integral instances
  randomRange :: MonadIO m => a -> a -> m a

instance UrhoRandom Int where
  random = liftIO $ fromIntegral <$> [C.exp| int {Random(2)} |]
  randomUp maxv = liftIO $ fromIntegral <$> [C.exp| int {Random($(int max'))} |]
    where max' = fromIntegral maxv
  randomRange minv maxv = liftIO $ fromIntegral <$> [C.exp| int {Random($(int min'), $(int max'))} |]
    where min' = fromIntegral minv
          max' = fromIntegral maxv

instance UrhoRandom Float where
  random = liftIO $ realToFrac <$> [C.exp| float {Random()} |]
  randomUp maxv = liftIO $ realToFrac <$> [C.exp| float {Random($(float max'))} |]
    where max' = realToFrac maxv
  randomRange minv maxv = liftIO $ realToFrac <$> [C.exp| float {Random($(float min'), $(float max'))} |]
    where min' = realToFrac minv
          max' = realToFrac maxv

-- | Convert float to half float. From https://gist.github.com/martinkallman/5049614
floatToHalf :: Float -> Word16
floatToHalf value = fromIntegral [C.pure| unsigned short {FloatToHalf($(float value'))} |]
  where value' = realToFrac value

-- | Convert half float to float. From https://gist.github.com/martinkallman/5049614
halfToFloat :: Word16 -> Float
halfToFloat value = realToFrac [C.pure| float {HalfToFloat($(unsigned short value'))} |]
  where value' = fromIntegral value
