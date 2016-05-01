{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Container.Vector.Common(
    PODVectorWord8
  , PODVectorWord
  , PODVectorMatrix3x4
  , VectorPODVectorWord
  , VectorPODVectorMatrix3x4
  , vectorContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Container.Vector.Internal.Common
import Graphics.Urho3D.Container.ForeignVector 
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> vectorCntx <> matrix3x4Context)
C.include "<Urho3D/Container/Vector.h>"
C.include "<Urho3D/Math/Matrix3x4.h>"
C.using "namespace Urho3D"

vectorContext :: C.Context 
vectorContext = vectorCntx 

C.verbatim "typedef PODVector<unsigned char> PODVectorWord8;"
C.verbatim "typedef PODVector<unsigned> PODVectorWord;"
C.verbatim "typedef PODVector<Matrix3x4> PODVectorMatrix3x4;;"
C.verbatim "typedef Vector<PODVector<unsigned> > VectorPODVectorWord;"
C.verbatim "typedef Vector<PODVector<Matrix3x4> > VectorPODVectorMatrix3x4;"

instance Createable (Ptr PODVectorWord8) where 
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


instance Createable (Ptr PODVectorWord) where 
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


instance Createable (Ptr PODVectorMatrix3x4) where 
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



instance Createable (Ptr VectorPODVectorWord) where 
  type CreationOptions (Ptr VectorPODVectorWord) = ()

  newObject _ = liftIO [C.exp| VectorPODVectorWord* { new Vector<PODVector<unsigned> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorPODVectorWord* ptr)} |]

instance ReadableVector VectorPODVectorWord where 
  type ReadVecElem VectorPODVectorWord = Ptr PODVectorWord

  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorPODVectorWord* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do 
    let i' = fromIntegral i 
    [C.exp| PODVectorWord* { &((*$(VectorPODVectorWord* ptr))[$(unsigned int i')]) } |]

instance WriteableVector VectorPODVectorWord where 
  type WriteVecElem VectorPODVectorWord = Ptr PODVectorWord 

  foreignVectorAppend ptr vptr = liftIO $ do
    [C.exp| void {$(VectorPODVectorWord* ptr)->Push(*$(PODVectorWord* vptr))} |]


instance Createable (Ptr VectorPODVectorMatrix3x4) where 
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