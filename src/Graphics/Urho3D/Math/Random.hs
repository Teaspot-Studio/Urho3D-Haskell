module Graphics.Urho3D.Math.Random(
    setRandomSeed
  , getRandomSeed
  , getRand
  , randStandardNormal
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Monad

C.context C.cppCtx
C.include "<Urho3D/Math/Random.h>"
C.using "namespace Urho3D"

-- | Set the random seed. The default seed is 1.
setRandomSeed :: MonadIO m => Int -> m ()
setRandomSeed seed = liftIO [C.exp| void { SetRandomSeed($(unsigned int seed')) } |]
  where seed' = fromIntegral seed

-- | Return the current random seed.
getRandomSeed :: MonadIO m => m Int 
getRandomSeed = liftIO $ fromIntegral <$> [C.exp| unsigned int { GetRandomSeed() } |]

-- | Return a random number between 0-32767. Should operate similarly to MSVC rand().
getRand :: MonadIO m => m Int 
getRand = liftIO $ fromIntegral <$> [C.exp| int { Rand() } |]

-- | Return a standard normal distributed number.
randStandardNormal :: MonadIO m => m Float 
randStandardNormal = liftIO $ realToFrac <$> [C.exp| float { RandStandardNormal() } |]