{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Geometry(
    Geometry
  , SharedGeometry
  , VectorSharedPtrGeometry
  , VectorVectorSharedPtrGeometry
  , geometryContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Control.Monad.IO.Class
import Data.Monoid
import Data.Vector (Vector)
import Foreign
import Graphics.Urho3D.Graphics.Internal.Geometry

import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> geometryCntx <> objectContext <> sharedGeometryPtrCntx)
C.include "<Urho3D/Graphics/Geometry.h>"
C.using "namespace Urho3D"

geometryContext :: C.Context
geometryContext = geometryCntx <> sharedGeometryPtrCntx

deriveParent ''Object ''Geometry

sharedPtr "Geometry"

C.verbatim "typedef Vector< SharedPtr<Geometry> > VectorSharedPtrGeometry;"

instance Creatable (Ptr VectorSharedPtrGeometry) where
  type CreationOptions (Ptr VectorSharedPtrGeometry) = ()
  newObject _ = liftIO [C.exp| VectorSharedPtrGeometry* {new Vector<SharedPtr<Geometry> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorSharedPtrGeometry* ptr) } |]

instance ReadableVector VectorSharedPtrGeometry where
  type ReadVecElem VectorSharedPtrGeometry = SharedPtr Geometry
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorSharedPtrGeometry* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peekSharedPtr =<< [C.exp| SharedGeometry* { new SharedPtr<Geometry>((*$(VectorSharedPtrGeometry* ptr))[$(unsigned int i')]) } |]
    where i' = fromIntegral i

instance WriteableVector VectorSharedPtrGeometry where
  type WriteVecElem VectorSharedPtrGeometry = SharedPtr Geometry
  foreignVectorAppend ptr e = liftIO $ withSharedPtr e $ \e' -> [C.exp| void {$(VectorSharedPtrGeometry* ptr)->Push(*$(SharedGeometry* e')) } |]

C.verbatim "typedef Vector<Vector< SharedPtr<Geometry> > > VectorVectorSharedPtrGeometry;"

instance Creatable (Ptr VectorVectorSharedPtrGeometry) where
  type CreationOptions (Ptr VectorVectorSharedPtrGeometry) = ()
  newObject _ = liftIO [C.exp| VectorVectorSharedPtrGeometry* {new VectorVectorSharedPtrGeometry() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorVectorSharedPtrGeometry* ptr) } |]

instance ReadableVector VectorVectorSharedPtrGeometry where
  type ReadVecElem VectorVectorSharedPtrGeometry = Vector (SharedPtr Geometry)
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorVectorSharedPtrGeometry* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peekForeignVectorAs =<< [C.exp| VectorSharedPtrGeometry* { &(*$(VectorVectorSharedPtrGeometry* ptr))[$(unsigned int i')] } |]
    where i' = fromIntegral i

instance WriteableVector VectorVectorSharedPtrGeometry where
  type WriteVecElem VectorVectorSharedPtrGeometry = Vector (SharedPtr Geometry)
  foreignVectorAppend ptr v = liftIO $ withForeignVector () v $ \v' -> [C.exp| void {$(VectorVectorSharedPtrGeometry* ptr)->Push(*$(VectorSharedPtrGeometry* v')) } |]
