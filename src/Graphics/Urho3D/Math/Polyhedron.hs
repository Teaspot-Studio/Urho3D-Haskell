{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Polyhedron(
    Polyhedron(..)
  , HasFaces(..)
  , polyhedronContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Math.Internal.Polyhedron
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Monad
import Text.RawString.QQ

C.context (C.cppCtx <> polyhedronCntx <> vector3Context)
C.include "<Urho3D/Math/Polyhedron.h>"
C.using "namespace Urho3D"

polyhedronContext :: C.Context 
polyhedronContext = polyhedronCntx

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

C.verbatim "typedef PODVector<Vector3> PODVectorVector3;"
C.verbatim "typedef Vector<PODVector<Vector3> > VectorPODVectorVector3;"

instance Storable Polyhedron where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Polyhedron) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Polyhedron>::AlignmentOf } |]
  peek ptr = do 
    _polyhedronFaces <- mapM peekForeignVectorAs =<< peekForeignVectorAs =<< [C.exp| VectorPODVectorVector3* { &$(Polyhedron* ptr)->faces_ } |]
    return Polyhedron{..}
  poke ptr Polyhedron{..} = withObject () $ \_polyhedronFaces' -> do 
    forM_ _polyhedronFaces $ \vface -> withForeignVector () vface $ foreignVectorAppend _polyhedronFaces' 
    [C.block| void { 
      *$(Polyhedron* ptr) = Polyhedron(*$(VectorPODVectorVector3* _polyhedronFaces'));
    } |]