{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Sphere(
    Sphere(..)
  , HasCenter(..)
  , HasRadius(..)
  , sphereContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign 
import Graphics.Urho3D.Math.Internal.Sphere
import Graphics.Urho3D.Math.Vector3
import Text.RawString.QQ

C.context (C.cppCtx <> sphereCntx <> vector3Context)
C.include "<Urho3D/Math/Sphere.h>"
C.using "namespace Urho3D"

sphereContext :: C.Context 
sphereContext = sphereCntx

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

instance Storable Sphere where 
  sizeOf _ = fromIntegral [C.pure| int { (int)sizeof(Sphere) } |]
  alignment _ = fromIntegral [C.pure| int { (int)Traits<Sphere>::AlignmentOf } |]
  peek ptr = do 
    _sphereCenter <- peek =<< [C.exp| Vector3* { &$(Sphere* ptr)->center_ } |]
    _sphereRadius <- realToFrac <$> [C.exp| float { $(Sphere* ptr)->radius_ } |]
    return Sphere{..}
  poke ptr Sphere{..} = 
    with _sphereCenter $ \_sphereCenter' ->
      [C.block| void { 
        $(Sphere* ptr)->center_ = *$(Vector3* _sphereCenter');
        $(Sphere* ptr)->radius_ = $(float _sphereRadius');
      } |]
    where
    _sphereRadius' = realToFrac _sphereRadius 