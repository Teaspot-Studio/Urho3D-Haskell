{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Frustum(
    Frustum(..)
  , HasNear(..)
  , HasLeft(..)
  , HasRight(..)
  , HasUp(..)
  , HasDown(..)
  , HasFar(..)
  , HasVert1(..)
  , HasVert2(..)
  , HasVert3(..)
  , HasVert4(..)
  , HasVert5(..)
  , HasVert6(..)
  , HasVert7(..)
  , HasVert8(..)
  , frustumContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign 
import Graphics.Urho3D.Math.Internal.Frustum
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Math.Plane
import Text.RawString.QQ

C.context (C.cppCtx <> frustumCntx <> vector3Context <> planeContext)
C.include "<Urho3D/Math/Frustum.h>"
C.using "namespace Urho3D"

frustumContext :: C.Context 
frustumContext = frustumCntx

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

instance Storable Frustum where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Frustum) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Frustum>::AlignmentOf } |]
  peek ptr = do 
    _frustumNear <- peek =<< [C.exp| Plane* { &$(Frustum* ptr)->planes_[PLANE_NEAR] } |]
    _frustumLeft <- peek =<< [C.exp| Plane* { &$(Frustum* ptr)->planes_[PLANE_LEFT] } |]
    _frustumRight <- peek =<< [C.exp| Plane* { &$(Frustum* ptr)->planes_[PLANE_RIGHT] } |]
    _frustumUp <- peek =<< [C.exp| Plane* { &$(Frustum* ptr)->planes_[PLANE_UP] } |]
    _frustumDown <- peek =<< [C.exp| Plane* { &$(Frustum* ptr)->planes_[PLANE_DOWN] } |]
    _frustumFar <- peek =<< [C.exp| Plane* { &$(Frustum* ptr)->planes_[PLANE_FAR] } |]

    _frustumVert1 <- peek =<< [C.exp| Vector3* { &$(Frustum* ptr)->vertices_[0] } |]
    _frustumVert2 <- peek =<< [C.exp| Vector3* { &$(Frustum* ptr)->vertices_[1] } |]
    _frustumVert3 <- peek =<< [C.exp| Vector3* { &$(Frustum* ptr)->vertices_[2] } |]
    _frustumVert4 <- peek =<< [C.exp| Vector3* { &$(Frustum* ptr)->vertices_[3] } |]
    _frustumVert5 <- peek =<< [C.exp| Vector3* { &$(Frustum* ptr)->vertices_[4] } |]
    _frustumVert6 <- peek =<< [C.exp| Vector3* { &$(Frustum* ptr)->vertices_[5] } |]
    _frustumVert7 <- peek =<< [C.exp| Vector3* { &$(Frustum* ptr)->vertices_[6] } |]
    _frustumVert8 <- peek =<< [C.exp| Vector3* { &$(Frustum* ptr)->vertices_[7] } |]

    return Frustum{..}
  poke ptr Frustum{..} = 
    with _frustumNear $ \_frustumNear' -> 
    with _frustumLeft $ \_frustumLeft' -> 
    with _frustumRight $ \_frustumRight' -> 
    with _frustumUp $ \_frustumUp' -> 
    with _frustumDown $ \_frustumDown' -> 
    with _frustumFar $ \_frustumFar' -> 
    with _frustumVert1 $ \_frustumVert1' -> 
    with _frustumVert2 $ \_frustumVert2' -> 
    with _frustumVert3 $ \_frustumVert3' -> 
    with _frustumVert4 $ \_frustumVert4' -> 
    with _frustumVert5 $ \_frustumVert5' -> 
    with _frustumVert6 $ \_frustumVert6' -> 
    with _frustumVert7 $ \_frustumVert7' -> 
    with _frustumVert8 $ \_frustumVert8' -> do
      [C.block| void { 
        $(Frustum* ptr)->planes_[PLANE_NEAR] = *$(Plane* _frustumNear');
        $(Frustum* ptr)->planes_[PLANE_LEFT] = *$(Plane* _frustumLeft');
        $(Frustum* ptr)->planes_[PLANE_RIGHT] = *$(Plane* _frustumRight');
        $(Frustum* ptr)->planes_[PLANE_UP] = *$(Plane* _frustumUp');
        $(Frustum* ptr)->planes_[PLANE_DOWN] = *$(Plane* _frustumDown');
        $(Frustum* ptr)->planes_[PLANE_FAR] = *$(Plane* _frustumFar');
        $(Frustum* ptr)->vertices_[0] = *$(Vector3* _frustumVert1');
        $(Frustum* ptr)->vertices_[1] = *$(Vector3* _frustumVert2');
        $(Frustum* ptr)->vertices_[2] = *$(Vector3* _frustumVert3');
        $(Frustum* ptr)->vertices_[3] = *$(Vector3* _frustumVert4');
        $(Frustum* ptr)->vertices_[4] = *$(Vector3* _frustumVert5');
        $(Frustum* ptr)->vertices_[5] = *$(Vector3* _frustumVert6');
        $(Frustum* ptr)->vertices_[6] = *$(Vector3* _frustumVert7');
        $(Frustum* ptr)->vertices_[7] = *$(Vector3* _frustumVert8');
      } |]