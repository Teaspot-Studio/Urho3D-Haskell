{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Color(
    Color(..)
  , rgb 
  , rgba
  , HasRComp(..)
  , HasGComp(..)
  , HasBComp(..)
  , HasAComp(..)
  , colorContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.Internal.Color
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Text.RawString.QQ
import Control.Lens 

C.context (C.cppCtx <> colorCntx)
C.include "<Urho3D/Math/Color.h>"
C.using "namespace Urho3D"

colorContext :: C.Context 
colorContext = colorCntx

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

-- | Helper to use default alpha
rgb :: Float -> Float -> Float -> Color 
rgb rc gc bc = Color rc gc bc 1

-- | Helpwer for Color
rgba :: Float -> Float -> Float -> Float -> Color 
rgba = Color 

instance Storable Color where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Color) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Color>::AlignmentOf } |]
  peek ptr = do 
    vr <- realToFrac <$> [C.exp| float { $(Color* ptr)->r_ } |]
    vg <- realToFrac <$> [C.exp| float { $(Color* ptr)->g_ } |]
    vb <- realToFrac <$> [C.exp| float { $(Color* ptr)->b_ } |]
    va <- realToFrac <$> [C.exp| float { $(Color* ptr)->a_ } |]
    return $ Color vr vg vb va
  poke ptr (Color vr vg vb va) = [C.block| void { 
    $(Color* ptr)->r_ = $(float vr');
    $(Color* ptr)->g_ = $(float vg');
    $(Color* ptr)->b_ = $(float vb');
    $(Color* ptr)->a_ = $(float va');
    } |]
    where
    vr' = realToFrac vr 
    vg' = realToFrac vg 
    vb' = realToFrac vb
    va' = realToFrac va

instance Num Color where 
  c1 + c2 = Color (c1^.rComp + c2^.rComp) (c1^.gComp + c2^.gComp) (c1^.bComp + c2^.bComp) (c1^.aComp + c2^.aComp)
  c1 - c2 = Color (c1^.rComp - c2^.rComp) (c1^.gComp - c2^.gComp) (c1^.bComp - c2^.bComp) (c1^.aComp - c2^.aComp)
  c1 * c2 = Color (c1^.rComp * c2^.rComp) (c1^.gComp * c2^.gComp) (c1^.bComp * c2^.bComp) (c1^.aComp * c2^.aComp)
  abs c = Color (abs $ c^.rComp) (abs $ c^.gComp) (abs $ c^.bComp) (abs $ c^.aComp) 
  signum c = Color (signum $ c^.rComp) (signum $ c^.gComp) (signum $ c^.bComp) (signum $ c^.aComp)
  fromInteger i = Color (fromIntegral i) (fromIntegral i) (fromIntegral i) 1

instance Creatable (Ptr Color) where
  type CreationOptions (Ptr Color) = Color

  newObject = liftIO . new
  deleteObject = liftIO . free