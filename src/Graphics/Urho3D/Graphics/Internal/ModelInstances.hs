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
