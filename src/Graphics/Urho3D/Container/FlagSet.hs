module Graphics.Urho3D.Container.FlagSet(
    FlagSet(..)
  , setFlag
  , unsetFlag
  , testFlag
  , flagsToList
  , flagsFromList
  ) where

import Control.DeepSeq
import Data.Bits
import Data.Foldable (foldl')
import Foreign
import GHC.Generics

-- | Wrap any 'impl' integer into bitfield which elements are values of 'val'.
--
-- Note that 'val' should have proper instance of Enum to not overlap.
newtype FlagSet impl val = FlagSet { unFlagSet :: impl }
  deriving (Eq, Ord, Show, Storable, Generic)

instance NFData impl => NFData (FlagSet impl val)

-- | Add flag to flag set
setFlag :: (Bits impl, Num impl, Enum val) => val -> FlagSet impl val -> FlagSet impl val
setFlag v (FlagSet fs) = FlagSet $ fs .|. fromIntegral (fromEnum v)

-- | Remove flag from flag set
unsetFlag :: (Bits impl, Num impl, Enum val) => val -> FlagSet impl val -> FlagSet impl val
unsetFlag v (FlagSet fs) = FlagSet $ fs .&. complement (fromIntegral (fromEnum v))

-- | Check that flag in the flag set is set
testFlag :: (Bits impl, Num impl, Eq impl, Enum val) => val -> FlagSet impl val -> Bool
testFlag v fs = unsetFlag v fs /= fs

-- | Get all set flags in flag set
flagsToList :: (Bits impl, Num impl, Eq impl, Enum val, Bounded val) => FlagSet impl val -> [val]
flagsToList fs = filter (`testFlag` fs) [minBound .. maxBound]

-- | Convert list of flags into compact field
flagsFromList :: (Foldable t, Bits impl, Num impl, Enum val) => t val -> FlagSet impl val
flagsFromList = foldl' (flip setFlag) (FlagSet 0)
