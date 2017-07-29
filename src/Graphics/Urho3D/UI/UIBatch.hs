{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.UIBatch(
    UIBatch(..)
  , PODVectorUIBatch
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
  , uiBatchPosAdjust
  , uiBatchSetColor
  , uiBatchSetDefaultColor
  , uiBatchAddQuad
  , uiBatchAddQuadMatrix
  , uiBatchAddQuadTexture
  , uiBatchAddQuadFreeform
  , uiBatchAddQuadFreeformMatrix
  , uiBatchMerge
  , batchMerge
  , uiBatchGetInterpolatedColor
  , uiBatchAddOrMerge
  , batchAddOrMerge
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.Vector.Unboxed as V

import Data.Monoid
import Foreign
import GHC.Generics
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.UIBatch
import System.IO.Unsafe (unsafePerformIO)
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
C.verbatim "typedef PODVector<UIBatch> PODVectorUIBatch;"

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

simplePODVector "UIBatch"

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

-- | Position adjustment vector for pixel-perfect rendering. Initialized by UI.
uiBatchPosAdjust :: MonadIO m => m Vector3
uiBatchPosAdjust = liftIO $ peek =<< [C.exp| Vector3* { &UIBatch::posAdjust } |]

-- | Set new color for the batch. Overrides gradient.
-- void SetColor(const Color& color, bool overrideAlpha = false);
uiBatchSetColor :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> Color -- ^ color
  -> Bool -- ^ override alpha (default false)
  -> m ()
uiBatchSetColor p c a = liftIO $ with c $ \c' -> do
  let ptr = parentPointer p
      a' = fromBool a
  [C.exp| void {$(UIBatch* ptr)->SetColor(*$(Color* c'), $(int a') != 0)} |]

-- | Restore UI element's default color.
-- void SetDefaultColor();
uiBatchSetDefaultColor :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> m ()
uiBatchSetDefaultColor p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(UIBatch* ptr)->SetDefaultColor()} |]

-- | Add a quad.
-- void AddQuad(int x, int y, int width, int height, int texOffsetX, int texOffsetY, int texWidth = 0, int texHeight = 0);
uiBatchAddQuad :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> Int -- ^ x
  -> Int -- ^ y
  -> Int -- ^ width
  -> Int -- ^ height
  -> Int -- ^ texOffsetX
  -> Int -- ^ texOffsetY
  -> Int -- ^ texWidth (default 0)
  -> Int -- ^ texHeight (default 0)
  -> m ()
uiBatchAddQuad p xv yv widthv heightv texOffsetX texOffsetY texWidth texHeight = liftIO $ do
  let ptr = parentPointer p
      xv' = fromIntegral xv
      yv' = fromIntegral yv
      widthv' = fromIntegral widthv
      heightv' = fromIntegral heightv
      texOffsetX' = fromIntegral texOffsetX
      texOffsetY' = fromIntegral texOffsetY
      texWidth' = fromIntegral texWidth
      texHeight' = fromIntegral texHeight
  [C.exp| void {$(UIBatch* ptr)->AddQuad(
      $(int xv')
    , $(int yv')
    , $(int widthv')
    , $(int heightv')
    , $(int texOffsetX')
    , $(int texOffsetY')
    , $(int texWidth')
    , $(int texHeight')
    )} |]

-- | Add a quad using a transform matrix.
-- void AddQuad(const Matrix3x4& transform, int x, int y, int width, int height, int texOffsetX, int texOffsetY, int texWidth = 0,
--     int texHeight = 0);
uiBatchAddQuadMatrix :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> Matrix3x4 -- ^ transform
  -> Int -- ^ x
  -> Int -- ^ y
  -> Int -- ^ width
  -> Int -- ^ height
  -> Int -- ^ texOffsetX
  -> Int -- ^ texOffsetY
  -> Int -- ^ texWidth (default 0)
  -> Int -- ^ texHeight (default 0)
  -> m ()
uiBatchAddQuadMatrix p m xv yv widthv heightv texOffsetX texOffsetY texWidth texHeight = liftIO $ with m $ \m' -> do
  let ptr = parentPointer p
      xv' = fromIntegral xv
      yv' = fromIntegral yv
      widthv' = fromIntegral widthv
      heightv' = fromIntegral heightv
      texOffsetX' = fromIntegral texOffsetX
      texOffsetY' = fromIntegral texOffsetY
      texWidth' = fromIntegral texWidth
      texHeight' = fromIntegral texHeight
  [C.exp| void {$(UIBatch* ptr)->AddQuad(
      *$(Matrix3x4* m')
    , $(int xv')
    , $(int yv')
    , $(int widthv')
    , $(int heightv')
    , $(int texOffsetX')
    , $(int texOffsetY')
    , $(int texWidth')
    , $(int texHeight')
    )} |]

-- | Add a quad with tiled texture.
-- void AddQuad(int x, int y, int width, int height, int texOffsetX, int texOffsetY, int texWidth, int texHeight, bool tiled);
uiBatchAddQuadTexture :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> Int -- ^ x
  -> Int -- ^ y
  -> Int -- ^ width
  -> Int -- ^ height
  -> Int -- ^ texOffsetX
  -> Int -- ^ texOffsetY
  -> Int -- ^ texWidth
  -> Int -- ^ texHeight
  -> Bool -- ^ tiled
  -> m ()
uiBatchAddQuadTexture p xv yv widthv heightv texOffsetX texOffsetY texWidth texHeight tiled = liftIO $ do
  let ptr = parentPointer p
      xv' = fromIntegral xv
      yv' = fromIntegral yv
      widthv' = fromIntegral widthv
      heightv' = fromIntegral heightv
      texOffsetX' = fromIntegral texOffsetX
      texOffsetY' = fromIntegral texOffsetY
      texWidth' = fromIntegral texWidth
      texHeight' = fromIntegral texHeight
      tiled' = fromBool tiled
  [C.exp| void {$(UIBatch* ptr)->AddQuad(
      $(int xv')
    , $(int yv')
    , $(int widthv')
    , $(int heightv')
    , $(int texOffsetX')
    , $(int texOffsetY')
    , $(int texWidth')
    , $(int texHeight')
    , $(int tiled') != 0
    )} |]

-- | Add a quad with freeform points and UVs. Uses the current color, not gradient. Points should be specified in clockwise order.
-- void AddQuad(const Matrix3x4& transform, const IntVector2& a, const IntVector2& b, const IntVector2& c, const IntVector2& d,
--     const IntVector2& texA, const IntVector2& texB, const IntVector2& texC, const IntVector2& texD);
uiBatchAddQuadFreeform :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> Matrix3x4 -- ^ transform
  -> IntVector2 -- ^ a
  -> IntVector2 -- ^ b
  -> IntVector2 -- ^ c
  -> IntVector2 -- ^ d
  -> IntVector2 -- ^ texA
  -> IntVector2 -- ^ texB
  -> IntVector2 -- ^ texC
  -> IntVector2 -- ^ texD
  -> m ()
uiBatchAddQuadFreeform p m av bv cv dv texA texB texC texD = liftIO $
  with m $ \m' ->
  with av $ \av' ->
  with bv $ \bv' ->
  with cv $ \cv' ->
  with dv $ \dv' ->
  with texA $ \texA' ->
  with texB $ \texB' ->
  with texC $ \texC' ->
  with texD $ \texD' -> do
  let ptr = parentPointer p
  [C.exp| void {$(UIBatch* ptr)->AddQuad(
      *$(Matrix3x4* m')
    , *$(IntVector2* av')
    , *$(IntVector2* bv')
    , *$(IntVector2* cv')
    , *$(IntVector2* dv')
    , *$(IntVector2* texA')
    , *$(IntVector2* texB')
    , *$(IntVector2* texC')
    , *$(IntVector2* texD')
    )} |]

-- | Add a quad with freeform points, UVs and colors. Points should be specified in clockwise order.
-- void AddQuad(const Matrix3x4& transform, const IntVector2& a, const IntVector2& b, const IntVector2& c, const IntVector2& d,
--     const IntVector2& texA, const IntVector2& texB, const IntVector2& texC, const IntVector2& texD, const Color& colA,
--     const Color& colB, const Color& colC, const Color& colD);
uiBatchAddQuadFreeformMatrix :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> Matrix3x4 -- ^ transform
  -> IntVector2 -- ^ a
  -> IntVector2 -- ^ b
  -> IntVector2 -- ^ c
  -> IntVector2 -- ^ d
  -> IntVector2 -- ^ texA
  -> IntVector2 -- ^ texB
  -> IntVector2 -- ^ texC
  -> IntVector2 -- ^ texD
  -> Color -- ^ colA
  -> Color -- ^ colB
  -> Color -- ^ colC
  -> Color -- ^ colD
  -> m ()
uiBatchAddQuadFreeformMatrix p m av bv cv dv texA texB texC texD colA colB colC colD = liftIO $
  with m $ \m' ->
  with av $ \av' ->
  with bv $ \bv' ->
  with cv $ \cv' ->
  with dv $ \dv' ->
  with texA $ \texA' ->
  with texB $ \texB' ->
  with texC $ \texC' ->
  with texD $ \texD' ->
  with colA $ \colA' ->
  with colB $ \colB' ->
  with colC $ \colC' ->
  with colD $ \colD' -> do
  let ptr = parentPointer p
  [C.exp| void {$(UIBatch* ptr)->AddQuad(
      *$(Matrix3x4* m')
    , *$(IntVector2* av')
    , *$(IntVector2* bv')
    , *$(IntVector2* cv')
    , *$(IntVector2* dv')
    , *$(IntVector2* texA')
    , *$(IntVector2* texB')
    , *$(IntVector2* texC')
    , *$(IntVector2* texD')
    , *$(Color* colA')
    , *$(Color* colB')
    , *$(Color* colC')
    , *$(Color* colD')
    )} |]

-- | Merge with another batch.
-- bool Merge(const UIBatch& batch);
uiBatchMerge :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> Ptr UIBatch -- ^ batch
  -> m Bool
uiBatchMerge p b = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(UIBatch* ptr)->Merge(*$(UIBatch* b))} |]

-- | Merge with another batch. Pure version.
batchMerge :: UIBatch -> UIBatch -> Maybe UIBatch
batchMerge b1 b2 = unsafePerformIO $ with b1 $ \b1' -> with b2 $ \b2' -> do
  res <- uiBatchMerge b1' b2'
  if res then Just <$> peek b1' else pure Nothing

-- | Return an interpolated color for the UI element.
-- unsigned GetInterpolatedColor(int x, int y);
uiBatchGetInterpolatedColor :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> Int -- ^ x
  -> Int -- ^ y
  -> m Word
uiBatchGetInterpolatedColor p xv yv = liftIO $ do
  let ptr = parentPointer p
      xv' = fromIntegral xv
      yv' = fromIntegral yv
  fromIntegral <$> [C.exp| unsigned int {$(UIBatch* ptr)->GetInterpolatedColor($(int xv'), $(int yv'))} |]

-- | Add or merge a batch.
-- static void AddOrMerge(const UIBatch& batch, PODVector<UIBatch>& batches);
uiBatchAddOrMerge :: (Parent UIBatch a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to uiBatch or ascentor
  -> Ptr PODVectorUIBatch
  -> m ()
uiBatchAddOrMerge p pv = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {UIBatch::AddOrMerge(*$(UIBatch* ptr), *$(PODVectorUIBatch* pv))} |]

-- | Add or merge a batch. Pure version
-- static void AddOrMerge(const UIBatch& batch, PODVector<UIBatch>& batches);
batchAddOrMerge :: ForeignVector v UIBatch
  => UIBatch -- ^ batch
  -> v UIBatch -- ^ batches
  -> v UIBatch
batchAddOrMerge b bs = unsafePerformIO $ with b $ \b' -> withForeignVector () bs $ \bs' -> do
  [C.exp| void {UIBatch::AddOrMerge(*$(UIBatch* b'), *$(PODVectorUIBatch* bs'))} |]
  peekForeignVectorAs bs'
