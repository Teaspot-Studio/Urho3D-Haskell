{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.ModelMorph(
    ModelMorph(..)
  , VertexBufferMorph(..)
  , HasName(..)
  , HasWeight(..)
  , HasBuffers(..)
  , HasElementMask(..)
  , HasElementVertexCount(..)
  , HasElementDataSize(..)
  , HasMorphData(..)
  , VectorModelMorph
  , modelMorphContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign 
import Foreign.C.String
import Graphics.Urho3D.Graphics.Internal.ModelMorph
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Monad
import Text.RawString.QQ
import Data.IORef 

import qualified Data.Vector.Unboxed as V 
import qualified Data.HashMap.Strict as H 

C.context (C.cppCtx <> C.funConstCtx <> modelMorphCntx)
C.include "<Urho3D/Graphics/Model.h>"
C.using "namespace Urho3D"

modelMorphContext :: C.Context 
modelMorphContext = modelMorphCntx

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

instance Storable VertexBufferMorph where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(VertexBufferMorph) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<VertexBufferMorph>::AlignmentOf } |]
  peek ptr = do 
    elMask <- fromIntegral <$> [C.exp| unsigned int { $(VertexBufferMorph* ptr)->elementMask_ } |]
    vertCount <- fromIntegral <$> [C.exp| unsigned int { $(VertexBufferMorph* ptr)->vertexCount_ } |]
    dSize <- fromIntegral <$> [C.exp| unsigned int { $(VertexBufferMorph* ptr)->dataSize_ } |]
    mData <- peekVector $ fromIntegral dSize
    return $ VertexBufferMorph elMask vertCount dSize mData
    where 
    peekVector n = do 
      mref <- newIORef $ V.replicate n 0 
      let setValue i v = modifyIORef' mref $ \vec -> V.unsafeUpd vec [(fromIntegral i, fromIntegral v)]
      [C.block| void {
        int n = $(VertexBufferMorph* ptr)->dataSize_;
        for(unsigned i = 0; i < n; i++) {
          $funConst:(void (*setValue)(unsigned int, unsigned char))(i, $(VertexBufferMorph* ptr)->morphData_[i]);
        }
      } |]
      readIORef mref
  poke ptr (VertexBufferMorph elMask vertCount dSize mData) = do 
    let n = fromIntegral $ V.length mData
    [C.block| void { 
      $(VertexBufferMorph* ptr)->elementMask_ = $(unsigned int elMask');
      $(VertexBufferMorph* ptr)->vertexCount_ = $(unsigned int vertCount');
      $(VertexBufferMorph* ptr)->dataSize_ = $(unsigned int dSize');
      $(VertexBufferMorph* ptr)->morphData_ = SharedArrayPtr<unsigned char>(new unsigned char[$(int n)]);
    } |]
    V.imapM_ (\i v -> do 
      let i' = fromIntegral i
          v' = fromIntegral v 
      [C.exp| void { 
        $(VertexBufferMorph* ptr)->morphData_[$(int i')] = $(unsigned char v')
      }|]) mData
    where
    elMask' = fromIntegral elMask 
    vertCount' = fromIntegral vertCount 
    dSize' = fromIntegral dSize

instance Storable ModelMorph where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(ModelMorph) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<ModelMorph>::AlignmentOf } |]
  peek ptr = do 
    mname <- peekCString =<< [C.exp| const char* { $(ModelMorph* ptr)->name_.CString() } |]
    mweight <- realToFrac <$> [C.exp| float { $(ModelMorph* ptr)->weight_ } |]
    mbuffers <- peekMap
    return $ ModelMorph mname mweight mbuffers
    where 
    peekMap = do 
      mref <- newIORef H.empty 
      let addValue k v = do
            v' <- peek v
            modifyIORef' mref $ H.insert (fromIntegral k) v'
      [C.block| void {
        HashMap<unsigned, VertexBufferMorph>::Iterator it = $(ModelMorph* ptr)->buffers_.Begin();
        while(it != $(ModelMorph* ptr)->buffers_.End()) {
          $funConst:(void (*addValue)(unsigned int, VertexBufferMorph*))(it->first_, &it->second_);
        }
      } |]
      readIORef mref
  poke ptr (ModelMorph mname mweight mbuffers) = withCString mname $ \name' -> do 
    [C.block| void { 
      $(ModelMorph* ptr)->name_ = String($(const char* name'));
      $(ModelMorph* ptr)->nameHash_ = StringHash($(const char* name'));
      $(ModelMorph* ptr)->weight_ = $(float weight');
      $(ModelMorph* ptr)->buffers_ = HashMap<unsigned, VertexBufferMorph>();
    } |]
    forM_ (H.toList mbuffers) $ \(k, v) -> with v $ \v' -> do 
      let k' = fromIntegral k
      [C.exp| void { 
        $(ModelMorph* ptr)->buffers_.Insert(Pair<unsigned, VertexBufferMorph>($(unsigned int k'), *$(VertexBufferMorph* v')))
      }|]
    where
    weight' = realToFrac mweight 

C.verbatim "typedef Vector<ModelMorph> VectorModelMorph;"

instance Createable (Ptr VectorModelMorph) where 
  type CreationOptions (Ptr VectorModelMorph) = ()
  newObject _ = liftIO [C.exp| VectorModelMorph* {new Vector<ModelMorph>() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorModelMorph* ptr) } |]

instance ReadableVector VectorModelMorph where 
  type ReadVecElem VectorModelMorph = ModelMorph
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorModelMorph* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peek =<< [C.exp| ModelMorph* { &(*$(VectorModelMorph* ptr))[$(unsigned int i')] } |]
    where i' = fromIntegral i 

instance WriteableVector VectorModelMorph where 
  type WriteVecElem VectorModelMorph = ModelMorph
  foreignVectorAppend ptr e = liftIO $ with e $ \e' -> [C.exp| void {$(VectorModelMorph* ptr)->Push(*$(ModelMorph* e')) } |]
