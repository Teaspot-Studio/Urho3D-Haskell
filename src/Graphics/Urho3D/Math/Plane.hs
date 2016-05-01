{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Plane(
    Plane(..)
  , HasNormal(..)
  , HasAbsNormal(..)
  , HasConstant(..)
  , planeContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign 
import Graphics.Urho3D.Math.Internal.Plane
import Graphics.Urho3D.Math.Vector3
import Text.RawString.QQ

C.context (C.cppCtx <> planeCntx <> vector3Context)
C.include "<Urho3D/Math/Plane.h>"
C.using "namespace Urho3D"

planeContext :: C.Context 
planeContext = planeCntx

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

instance Storable Plane where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Plane) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Plane>::AlignmentOf } |]
  peek ptr = do 
    _planeNormal <- peek =<< [C.exp| Vector3* { &$(Plane* ptr)->normal_ } |]
    _planeAbsNormal <- peek =<< [C.exp| Vector3* { &$(Plane* ptr)->absNormal_ } |]
    _planeConstant <- realToFrac <$> [C.exp| float { $(Plane* ptr)->d_ } |]
    return Plane{..}
  poke ptr Plane{..} = 
    with _planeNormal $ \_planeNormal' ->
    with _planeAbsNormal $ \_planeAbsNormal' -> do 
      [C.block| void { 
        $(Plane* ptr)->normal_ = *$(Vector3* _planeNormal');
        $(Plane* ptr)->absNormal_ = *$(Vector3* _planeAbsNormal');
        $(Plane* ptr)->d_ = $(float _planeConstant');
      } |]
    where
    _planeConstant' = realToFrac _planeConstant 