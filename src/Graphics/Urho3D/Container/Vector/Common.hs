{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Graphics.Urho3D.Container.Vector.Common(
    PODVectorWord8
  , PODVectorWord
  , PODVectorMatrix3x4
  , PODVectorBool
  , PODVectorFloat
  , PODVectorInt
  , PODVectorBillboard
  , PODVectorVertexElement
  , VectorPODVectorWord
  , VectorPODVectorMatrix3x4
  , SharedArrayWord8
  , vectorContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Data.Vector (Vector)
import Foreign
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Container.Vector.Internal.Common
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Graphics.Internal.BillboardSet
import Graphics.Urho3D.Graphics.Internal.BillboardSetInstances()
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Monad

C.context (C.cppCtx <> vectorCntx <> matrix3x4Context <> billboardSetCntx <> sharedArrayWord8PtrCntx <> podVectorVertexElementCntx <> graphDefsContext)
C.include "<Urho3D/Container/Vector.h>"
C.include "<Urho3D/Container/ArrayPtr.h>"
C.include "<Urho3D/Math/Matrix3x4.h>"
C.include "<Urho3D/Graphics/BillboardSet.h>"
C.include "<Urho3D/Graphics/GraphicsDefs.h>"
C.using "namespace Urho3D"

vectorContext :: C.Context
vectorContext = vectorCntx <> sharedArrayWord8PtrCntx <> podVectorVertexElementCntx

C.verbatim "typedef PODVector<unsigned char> PODVectorWord8;"
C.verbatim "typedef PODVector<unsigned> PODVectorWord;"
C.verbatim "typedef PODVector<Matrix3x4> PODVectorMatrix3x4;"
C.verbatim "typedef PODVector<bool> PODVectorBool;"
C.verbatim "typedef PODVector<float> PODVectorFloat;"
C.verbatim "typedef PODVector<int> PODVectorInt;"
C.verbatim "typedef PODVector<Billboard> PODVectorBillboard;"
C.verbatim "typedef Vector<PODVector<unsigned> > VectorPODVectorWord;"
C.verbatim "typedef Vector<PODVector<Matrix3x4> > VectorPODVectorMatrix3x4;"

instance Creatable (Ptr PODVectorWord8) where
  type CreationOptions (Ptr PODVectorWord8) = ()

  newObject _ = liftIO [C.exp| PODVectorWord8* { new PODVector<unsigned char>() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(PODVectorWord8* ptr)} |]

instance ReadableVector PODVectorWord8 where
  type ReadVecElem PODVectorWord8 = Word8

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(PODVectorWord8* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    fromIntegral <$> [C.exp| unsigned char { (*$(PODVectorWord8* ptr))[$(unsigned int i')] } |]

instance WriteableVector PODVectorWord8 where
  type WriteVecElem PODVectorWord8 = Word8

  foreignVectorAppend ptr w = liftIO $ do
    let w' = fromIntegral w
    [C.exp| void {$(PODVectorWord8* ptr)->Push($(unsigned char w'))} |]


instance Creatable (Ptr PODVectorWord) where
  type CreationOptions (Ptr PODVectorWord) = ()

  newObject _ = liftIO [C.exp| PODVectorWord* { new PODVector<unsigned int>() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(PODVectorWord* ptr)} |]

instance ReadableVector PODVectorWord where
  type ReadVecElem PODVectorWord = Word

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(PODVectorWord* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    fromIntegral <$> [C.exp| unsigned int { (*$(PODVectorWord* ptr))[$(unsigned int i')] } |]

instance WriteableVector PODVectorWord where
  type WriteVecElem PODVectorWord = Word

  foreignVectorAppend ptr w = liftIO $ do
    let w' = fromIntegral w
    [C.exp| void {$(PODVectorWord* ptr)->Push($(unsigned int w'))} |]


instance Creatable (Ptr PODVectorMatrix3x4) where
  type CreationOptions (Ptr PODVectorMatrix3x4) = ()

  newObject _ = liftIO [C.exp| PODVectorMatrix3x4* { new PODVector<Matrix3x4>() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(PODVectorMatrix3x4* ptr)} |]

instance ReadableVector PODVectorMatrix3x4 where
  type ReadVecElem PODVectorMatrix3x4 = Matrix3x4

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(PODVectorMatrix3x4* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    peek =<< [C.exp| Matrix3x4* { &((*$(PODVectorMatrix3x4* ptr))[$(unsigned int i')]) } |]

instance WriteableVector PODVectorMatrix3x4 where
  type WriteVecElem PODVectorMatrix3x4 = Matrix3x4

  foreignVectorAppend ptr mtx = liftIO $ with mtx $ \mtx' -> do
    [C.exp| void {$(PODVectorMatrix3x4* ptr)->Push(*$(Matrix3x4* mtx'))} |]



instance Creatable (Ptr PODVectorBool) where
  type CreationOptions (Ptr PODVectorBool) = ()

  newObject _ = liftIO [C.exp| PODVectorBool* { new PODVector<bool>() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(PODVectorBool* ptr)} |]

instance ReadableVector PODVectorBool where
  type ReadVecElem PODVectorBool = Bool

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(PODVectorBool* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    toBool <$> [C.exp| int { (int)(*$(PODVectorBool* ptr))[$(unsigned int i')] } |]

instance WriteableVector PODVectorBool where
  type WriteVecElem PODVectorBool = Bool

  foreignVectorAppend ptr w = liftIO $ do
    let w' = fromBool w
    [C.exp| void {$(PODVectorBool* ptr)->Push($(int w') != 0)} |]

instance Creatable (Ptr PODVectorFloat) where
  type CreationOptions (Ptr PODVectorFloat) = ()

  newObject _ = liftIO [C.exp| PODVectorFloat* { new PODVector<float>() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(PODVectorFloat* ptr)} |]

instance ReadableVector PODVectorFloat where
  type ReadVecElem PODVectorFloat = Float

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(PODVectorFloat* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    realToFrac <$> [C.exp| float { (*$(PODVectorFloat* ptr))[$(unsigned int i')] } |]

instance WriteableVector PODVectorFloat where
  type WriteVecElem PODVectorFloat = Float

  foreignVectorAppend ptr w = liftIO $ do
    let w' = realToFrac w
    [C.exp| void {$(PODVectorFloat* ptr)->Push($(float w'))} |]



instance Creatable (Ptr PODVectorInt) where
  type CreationOptions (Ptr PODVectorInt) = ()

  newObject _ = liftIO [C.exp| PODVectorInt* { new PODVector<int>() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(PODVectorInt* ptr)} |]

instance ReadableVector PODVectorInt where
  type ReadVecElem PODVectorInt = Int

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(PODVectorInt* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    fromIntegral <$> [C.exp| int { (*$(PODVectorInt* ptr))[$(unsigned int i')] } |]

instance WriteableVector PODVectorInt where
  type WriteVecElem PODVectorInt = Int

  foreignVectorAppend ptr w = liftIO $ do
    let w' = fromIntegral w
    [C.exp| void {$(PODVectorInt* ptr)->Push($(int w'))} |]


instance Creatable (Ptr PODVectorBillboard) where
  type CreationOptions (Ptr PODVectorBillboard) = ()

  newObject _ = liftIO [C.exp| PODVectorBillboard* { new PODVector<Billboard>() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(PODVectorBillboard* ptr)} |]

instance ReadableVector PODVectorBillboard where
  type ReadVecElem PODVectorBillboard = Billboard

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(PODVectorBillboard* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    peek =<< [C.exp| Billboard* { &(*$(PODVectorBillboard* ptr))[$(unsigned int i')] } |]

instance WriteableVector PODVectorBillboard where
  type WriteVecElem PODVectorBillboard = Billboard

  foreignVectorAppend ptr w = liftIO $ with w $ \w' -> do
    [C.exp| void {$(PODVectorBillboard* ptr)->Push(*$(Billboard* w'))} |]


instance Creatable (Ptr VectorPODVectorWord) where
  type CreationOptions (Ptr VectorPODVectorWord) = ()

  newObject _ = liftIO [C.exp| VectorPODVectorWord* { new Vector<PODVector<unsigned> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorPODVectorWord* ptr)} |]

instance ReadableVector VectorPODVectorWord where
  type ReadVecElem VectorPODVectorWord = Vector Word

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorPODVectorWord* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    peekForeignVectorAs =<< [C.exp| PODVectorWord* { &((*$(VectorPODVectorWord* ptr))[$(unsigned int i')]) } |]

instance WriteableVector VectorPODVectorWord where
  type WriteVecElem VectorPODVectorWord = Vector Word

  foreignVectorAppend ptr v = liftIO $ withForeignVector () v $ \v' -> do
    [C.exp| void {$(VectorPODVectorWord* ptr)->Push(*$(PODVectorWord* v'))} |]


instance Creatable (Ptr VectorPODVectorMatrix3x4) where
  type CreationOptions (Ptr VectorPODVectorMatrix3x4) = ()

  newObject _ = liftIO [C.exp| VectorPODVectorMatrix3x4* { new Vector<PODVector<Matrix3x4> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorPODVectorMatrix3x4* ptr)} |]

instance ReadableVector VectorPODVectorMatrix3x4 where
  type ReadVecElem VectorPODVectorMatrix3x4 = Ptr PODVectorMatrix3x4

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorPODVectorMatrix3x4* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    [C.exp| PODVectorMatrix3x4* { &((*$(VectorPODVectorMatrix3x4* ptr))[$(unsigned int i')]) } |]

instance WriteableVector VectorPODVectorMatrix3x4 where
  type WriteVecElem VectorPODVectorMatrix3x4 = Ptr PODVectorMatrix3x4

  foreignVectorAppend ptr vptr = liftIO $ do
    [C.exp| void {$(VectorPODVectorMatrix3x4* ptr)->Push(*$(PODVectorMatrix3x4* vptr))} |]

sharedArrayPtr "unsigned char" "Word8"

simplePODVector "VertexElement"
