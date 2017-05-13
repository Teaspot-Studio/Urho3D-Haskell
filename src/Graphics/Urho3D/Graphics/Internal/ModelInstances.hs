{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Internal.ModelInstances(

  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Text.RawString.QQ

import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Graphics.Internal.Model

C.context (C.cppCtx <> modelCntx <> vectorContext)
C.include "<Urho3D/Graphics/Model.h>"
C.using "namespace Urho3D"

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

C.verbatim "typedef SharedArrayPtr<unsigned char> SharedArrayWord8;"
C.verbatim "typedef PODVector<VertexElement> PODVectorVertexElement;"

instance Storable VertexBufferDesc where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(VertexBufferDesc) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<VertexBufferDesc>::AlignmentOf } |]
  peek ptr = do
    _vertexBufferDescVertexCount <- fromIntegral <$> [C.exp| unsigned int { $(VertexBufferDesc* ptr)->vertexCount_ } |]
    _vertexBufferDescVertexElements <- peekForeignVectorAs =<< [C.exp| PODVectorVertexElement* { &$(VertexBufferDesc* ptr)->vertexElements_ } |]
    dataSize <- fromIntegral <$> [C.exp| unsigned int { $(VertexBufferDesc* ptr)->dataSize_ } |]
    _vertexBufferDescDatum <- peekSharedArrayPtr dataSize =<< [C.exp| SharedArrayWord8* { new SharedArrayWord8($(VertexBufferDesc* ptr)->data_) } |]
    pure VertexBufferDesc{..}
  poke ptr VertexBufferDesc{..} = withSharedArrayPtr _vertexBufferDescDatum $ \n datum' ->
    withForeignVector () _vertexBufferDescVertexElements $ \v -> do
    let vcount = fromIntegral _vertexBufferDescVertexCount
        n' = fromIntegral n
    [C.block| void {
      VertexBufferDesc *ptr = $(VertexBufferDesc* ptr);
      ptr->vertexCount_ = $(unsigned int vcount);
      ptr->vertexElements_ = *$(PODVectorVertexElement* v);
      ptr->dataSize_ = $(unsigned int n');
      ptr->data_ = *$(SharedArrayWord8* datum');
    }|]

instance Storable IndexBufferDesc where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(IndexBufferDesc) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<IndexBufferDesc>::AlignmentOf } |]
  peek ptr = do
    _indexBufferDescIndexCount <- fromIntegral <$> [C.exp| unsigned int { $(IndexBufferDesc* ptr)->indexCount_ } |]
    _indexBufferDescIndexSize <- fromIntegral <$> [C.exp| unsigned int { $(IndexBufferDesc* ptr)->indexSize_ } |]
    dataSize <- fromIntegral <$> [C.exp| unsigned int { $(IndexBufferDesc* ptr)->dataSize_ } |]
    _indexBufferDescDatum <- peekSharedArrayPtr dataSize =<< [C.exp| SharedArrayWord8* { new SharedArrayWord8($(IndexBufferDesc* ptr)->data_) } |]
    pure IndexBufferDesc {..}
  poke ptr IndexBufferDesc{..} = withSharedArrayPtr _indexBufferDescDatum $ \n datum' -> do
    let ic = fromIntegral _indexBufferDescIndexCount
        is = fromIntegral _indexBufferDescIndexSize
        n' = fromIntegral n
    [C.block| void {
      IndexBufferDesc *ptr = $(IndexBufferDesc* ptr);
      ptr->indexCount_ = $(unsigned int ic);
      ptr->indexSize_ = $(unsigned int is);
      ptr->dataSize_ = $(unsigned int n');
      ptr->data_ = *$(SharedArrayWord8* datum');
    }|]

instance Storable GeometryDesc where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(GeometryDesc) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<GeometryDesc>::AlignmentOf } |]
  peek ptr = do
    _geometryDescPType <- toEnum . fromIntegral <$> [C.exp| int { (int)$(GeometryDesc* ptr)->type_ } |]
    _geometryDescVbRef <- fromIntegral <$> [C.exp| unsigned int { $(GeometryDesc* ptr)->vbRef_ } |]
    _geometryDescIbRef <- fromIntegral <$> [C.exp| unsigned int { $(GeometryDesc* ptr)->ibRef_ } |]
    _geometryDescIndexStart <- fromIntegral <$> [C.exp| unsigned int { $(GeometryDesc* ptr)->indexStart_ } |]
    _geometryDescIndexCount <- fromIntegral <$> [C.exp| unsigned int { $(GeometryDesc* ptr)->indexCount_ } |]
    pure GeometryDesc {..}
  poke ptr GeometryDesc{..} = do
    let type_ = fromIntegral . fromEnum $ _geometryDescPType
        vbRef_ = fromIntegral _geometryDescVbRef
        ibRef_ = fromIntegral _geometryDescIbRef
        indexStart_ = fromIntegral _geometryDescIndexStart
        indexCount_ = fromIntegral _geometryDescIndexCount
    [C.block| void {
      GeometryDesc *ptr = $(GeometryDesc* ptr);
      ptr->type_ = (PrimitiveType)$(int type_);
      ptr->vbRef_ = $(unsigned int vbRef_);
      ptr->ibRef_ = $(unsigned int ibRef_);
      ptr->indexStart_ = $(unsigned int indexStart_);
      ptr->indexCount_ = $(unsigned int indexCount_);
    }|]
