{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Graphics.DecalSet(
    DecalSet
  , DecalVertex(..)
  , PODVectorDecalVertex
  , HasPosition(..)
  , HasNormal(..)
  , HasTexCoord(..)
  , HasTangent(..)
  , HasBlendWeights0(..)
  , HasBlendWeights1(..)
  , HasBlendWeights2(..)
  , HasBlendWeights3(..)
  , HasBlendIndices0(..)
  , HasBlendIndices1(..)
  , HasBlendIndices2(..)
  , HasBlendIndices3(..)
  , Decal(..)
  , HasTimer(..)
  , HasTimeToLive(..)
  , HasBoundingBox(..)
  , HasVertecies(..)
  , HasIndices(..)
  , decalSetContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Graphics.Internal.DecalSet
--import Graphics.Urho3D.Monad
import Graphics.Urho3D.Scene.Node
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ

import Graphics.Urho3D.Core.Object
--import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Graphics.Drawable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Math.Vector4
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx
  <> decalSetCntx
  <> componentContext
  <> animatableContext
  <> serializableContext
  <> drawableContext
  <> objectContext
  <> vector3Context
  <> vector2Context
  <> vector4Context
  )
C.include "<Urho3D/Graphics/DecalSet.h>"
C.using "namespace Urho3D"

C.verbatim "typedef PODVector<DecalVertex> PODVectorDecalVertex;"

simplePODVector "DecalVertex"

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

decalSetContext :: C.Context
decalSetContext = componentContext <> decalSetCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Drawable] ''DecalSet

instance NodeComponent DecalSet where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { DecalSet::GetTypeStatic().Value() } |]

instance Storable DecalVertex where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(DecalVertex) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<DecalVertex>::AlignmentOf } |]
  peek ptr = do
      _decalVertexPosition <- peek =<< [C.exp| Vector3* { &$(DecalVertex* ptr)->position_ } |]
      _decalVertexNormal <- peek =<< [C.exp| Vector3* { &$(DecalVertex* ptr)->normal_ } |]
      _decalVertexTexCoord <- peek =<< [C.exp| Vector2* { &$(DecalVertex* ptr)->texCoord_ } |]
      _decalVertexTangent <- peek =<< [C.exp| Vector4* { &$(DecalVertex* ptr)->tangent_ } |]
      _decalVertexBlendWeights0 <- realToFrac <$> [C.exp| float { $(DecalVertex* ptr)->blendWeights_[0] } |]
      _decalVertexBlendWeights1 <- realToFrac <$> [C.exp| float { $(DecalVertex* ptr)->blendWeights_[1] } |]
      _decalVertexBlendWeights2 <- realToFrac <$> [C.exp| float { $(DecalVertex* ptr)->blendWeights_[2] } |]
      _decalVertexBlendWeights3 <- realToFrac <$> [C.exp| float { $(DecalVertex* ptr)->blendWeights_[3] } |]
      _decalVertexBlendIndices0 <- fromIntegral <$> [C.exp| unsigned char { $(DecalVertex* ptr)->blendIndices_[0] } |]
      _decalVertexBlendIndices1 <- fromIntegral <$> [C.exp| unsigned char { $(DecalVertex* ptr)->blendIndices_[1] } |]
      _decalVertexBlendIndices2 <- fromIntegral <$> [C.exp| unsigned char { $(DecalVertex* ptr)->blendIndices_[2] } |]
      _decalVertexBlendIndices3 <- fromIntegral <$> [C.exp| unsigned char { $(DecalVertex* ptr)->blendIndices_[3] } |]
      return DecalVertex {..}
  poke ptr DecalVertex {..} =
    with _decalVertexPosition $ \_decalVertexPosition' ->
    with _decalVertexPosition $ \_decalVertexPosition' ->
    with _decalVertexNormal $ \_decalVertexNormal' ->
    with _decalVertexTexCoord $ \_decalVertexTexCoord' ->
    with _decalVertexTangent $ \_decalVertexTangent' ->
    [C.block| void {
      $(DecalVertex* ptr)->position_ = *$(Vector3* _decalVertexPosition');
      $(DecalVertex* ptr)->normal_ = *$(Vector3* _decalVertexNormal');
      $(DecalVertex* ptr)->texCoord_ = *$(Vector2* _decalVertexTexCoord');
      $(DecalVertex* ptr)->tangent_ = *$(Vector4* _decalVertexTangent');
      $(DecalVertex* ptr)->blendWeights_[0] = $(float _decalVertexBlendWeights0');
      $(DecalVertex* ptr)->blendWeights_[1] = $(float _decalVertexBlendWeights1');
      $(DecalVertex* ptr)->blendWeights_[2] = $(float _decalVertexBlendWeights2');
      $(DecalVertex* ptr)->blendWeights_[3] = $(float _decalVertexBlendWeights3');
      $(DecalVertex* ptr)->blendIndices_[0] = $(unsigned char _decalVertexBlendIndices0');
      $(DecalVertex* ptr)->blendIndices_[1] = $(unsigned char _decalVertexBlendIndices1');
      $(DecalVertex* ptr)->blendIndices_[2] = $(unsigned char _decalVertexBlendIndices2');
      $(DecalVertex* ptr)->blendIndices_[3] = $(unsigned char _decalVertexBlendIndices3');
     } |]
    where
    _decalVertexBlendWeights0' = realToFrac _decalVertexBlendWeights0
    _decalVertexBlendWeights1' = realToFrac _decalVertexBlendWeights1
    _decalVertexBlendWeights2' = realToFrac _decalVertexBlendWeights2
    _decalVertexBlendWeights3' = realToFrac _decalVertexBlendWeights3
    _decalVertexBlendIndices0' = fromIntegral _decalVertexBlendIndices0
    _decalVertexBlendIndices1' = fromIntegral _decalVertexBlendIndices1
    _decalVertexBlendIndices2' = fromIntegral _decalVertexBlendIndices2
    _decalVertexBlendIndices3' = fromIntegral _decalVertexBlendIndices3


-- -- | Return vertical field of view in degrees.
-- decalSetGetFov :: (Parent DecalSet a, Pointer p a, MonadIO m) => p -- ^ DecalSet pointer or child
--   -> m Float
-- decalSetGetFov p = liftIO $ do
--   let ptr = parentPointer p
--   realToFrac <$> [C.exp| float {$(DecalSet* ptr)->GetFov()} |]
