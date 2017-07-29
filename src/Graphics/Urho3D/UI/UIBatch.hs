{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.UIBatch(
    UIBatch(..)
  , HasElement(..)
  , HasBlendMode(..)
  , HasScissor(..)
  , HasTexture(..)
  , HasInvTextureSize(..)
  , HasColor(..)
  , HasVertexData(..)
  , HasVertexStart(..)
  , HasVertexEnd(..)
  , HasUseGradient(..)
  , uiBatchContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.Vector.Unboxed as V

import Data.Monoid
import Foreign
import GHC.Generics
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.UIBatch
import Text.RawString.QQ

import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.Matrix3x4
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx
  <> uiBatchCntx
  <> colorContext
  <> matrix3x4Context
  <> rectContext
  <> textureContext
  <> uiElementContext
  <> vector2Context
  <> vector3Context
  <> vectorContext
  )
C.include "<Urho3D/Container/Ptr.h>"
C.include "<Urho3D/UI/UIBatch.h>"
C.using "namespace Urho3D"

C.verbatim "typedef PODVector<float> PODVectorFloat;"

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

uiBatchContext :: C.Context
uiBatchContext = uiBatchCntx

-- | Construction options for 'UIBatch'
data UIBatchCreate = UIBatchDefault -- ^ Construct with defaults
  -- | Construct
  | UIBatchConstruct {
      _uiBatchConstructElement    :: !(Ptr UIElement)
    , _uiBatchConstructBlendMode  :: !BlendMode
    , _uiBatchConstructScissor    :: !IntRect
    , _uiBatchConstructTexture    :: !(Ptr Texture)
    , _uiBatchConstructVertexData :: !(V.Vector Float)
    }
  deriving (Show, Generic)

-- | Note that vertex data vector is copied when 'UIBatchConstruct' is used. The
-- vector is freed in 'deleteObject', so the deleteObject implementation might be
-- unsafe to use on non-haskell created 'UIBatch'.
instance Creatable (Ptr UIBatch) where
  type CreationOptions (Ptr UIBatch) = UIBatchCreate

  newObject UIBatchDefault = liftIO $ [C.exp| UIBatch* { new UIBatch() } |]
  newObject UIBatchConstruct{..} = liftIO $
    with _uiBatchConstructScissor $ \_uiBatchConstructScissor' ->
    withForeignVector () _uiBatchConstructVertexData $ \_uiBatchConstructVertexData' -> do
    let _uiBatchConstructBlendMode' = fromIntegral . fromEnum $ _uiBatchConstructBlendMode
    [C.exp| UIBatch* { new UIBatch(
        $(UIElement* _uiBatchConstructElement)
      , (BlendMode)$(int _uiBatchConstructBlendMode')
      , *$(IntRect* _uiBatchConstructScissor')
      , $(Texture* _uiBatchConstructTexture)
      , new PODVectorFloat(*$(PODVectorFloat* _uiBatchConstructVertexData'))
      ) } |]

  deleteObject ptr = liftIO $ [C.block| void {
      delete $(UIBatch* ptr)->vertexData_;
      delete $(UIBatch* ptr);
    } |]

-- | Note that vertex data vector is copied when poked. Use 'deleteObject' to free memory
-- including the copied data.
instance Storable UIBatch where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(UIBatch) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<UIBatch>::AlignmentOf } |]
  peek ptr = do
    _uIBatchElement <- [C.exp| UIElement* {$(UIBatch* ptr)->element_} |]
    _uIBatchBlendMode <- toEnum . fromIntegral <$> [C.exp| int {(int)$(UIBatch* ptr)->blendMode_} |]
    _uIBatchScissor <- peek =<< [C.exp| IntRect* {&$(UIBatch* ptr)->scissor_} |]
    _uIBatchTexture <- [C.exp| Texture* {$(UIBatch* ptr)->texture_} |]
    _uIBatchInvTextureSize <- peek =<< [C.exp| Vector2* {&$(UIBatch* ptr)->invTextureSize_} |]
    _uIBatchColor <- fromIntegral <$> [C.exp| unsigned int {$(UIBatch* ptr)->color_} |]
    _uIBatchVertexData <- peekForeignVectorAs =<< [C.exp| PODVectorFloat* {$(UIBatch* ptr)->vertexData_} |]
    _uIBatchVertexStart <- fromIntegral <$> [C.exp| unsigned int {$(UIBatch* ptr)->vertexStart_} |]
    _uIBatchVertexEnd <- fromIntegral <$> [C.exp| unsigned int {$(UIBatch* ptr)->vertexEnd_} |]
    _uIBatchUseGradient <- toBool <$> [C.exp| int {(int)$(UIBatch* ptr)->useGradient_} |]
    return UIBatch{..}
  poke ptr UIBatch{..} =
    with _uIBatchScissor $ \_uIBatchScissor' ->
    with _uIBatchInvTextureSize $ \_uIBatchInvTextureSize' ->
    withForeignVector () _uIBatchVertexData $ \_uIBatchVertexData' -> do
      let _uIBatchBlendMode' = fromIntegral . fromEnum $ _uIBatchBlendMode
          _uIBatchColor' = fromIntegral _uIBatchColor
          _uIBatchVertexStart' = fromIntegral _uIBatchVertexStart
          _uIBatchVertexEnd' = fromIntegral _uIBatchVertexEnd
          _uIBatchUseGradient' = fromBool _uIBatchUseGradient
      [C.block| void {
        $(UIBatch* ptr)->element_ = $(UIElement* _uIBatchElement);
        $(UIBatch* ptr)->blendMode_ = (BlendMode)$(int _uIBatchBlendMode');
        $(UIBatch* ptr)->scissor_ = *$(IntRect* _uIBatchScissor');
        $(UIBatch* ptr)->texture_ = $(Texture* _uIBatchTexture);
        $(UIBatch* ptr)->invTextureSize_ = *$(Vector2* _uIBatchInvTextureSize');
        $(UIBatch* ptr)->color_ = $(unsigned int _uIBatchColor');
        $(UIBatch* ptr)->vertexData_ = new PODVectorFloat(*$(PODVectorFloat* _uIBatchVertexData'));
        $(UIBatch* ptr)->vertexStart_ = $(unsigned int _uIBatchVertexStart');
        $(UIBatch* ptr)->vertexEnd_ = $(unsigned int _uIBatchVertexEnd');
        $(UIBatch* ptr)->useGradient_ = $(int _uIBatchUseGradient') != 0;
      } |]

-- -- | Set offset to image rectangle used when pressed.
-- -- void SetPressedOffset(const IntVector2& offset);
-- uiBatchSetPressedOffset :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
--   => ptr -- ^ Pointer to uiBatch or ascentor
--   -> m ()
-- uiBatchSetPressedOffset p = liftIO $ do
--   let ptr = parentPointer p
--   [C.exp| void {$(UIBatch* ptr)->SetPressedOffset()} |]
