{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.FontFace(
    FontFace
  , fontFaceContext
  , FontGlyph(..)
  , HasX(..)
  , HasY(..)
  , HasWidth(..)
  , HasHeight(..)
  , HasOffsetX(..)
  , HasOffsetY(..)
  , HasAdvanceX(..)
  , HasPage(..)
  , HasUsed(..)
  , fontFaceGetKerning
  , fontFaceIsDataLost
  , fontFaceGetPointSize
  , fontFaceGetRowHeight
  , fontFaceGetTextures
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Graphics.Texture2D
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.FontFace
import Text.RawString.QQ

C.context (C.cppCtx
  <> fontFaceCntx
  <> texture2DContext
  )
C.include "<Urho3D/Container/Ptr.h>"
C.include "<Urho3D/Container/HashMap.h>"
C.include "<Urho3D/UI/FontFace.h>"
C.using "namespace Urho3D"

fontFaceContext :: C.Context
fontFaceContext = fontFaceCntx

C.verbatim "typedef Vector<SharedPtr<Texture2D> > VectorSharedTexture2DPtr;"

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

instance Storable FontGlyph where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(FontGlyph) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<FontGlyph>::AlignmentOf } |]
  peek ptr = do
    _fontGlyphX <- fromIntegral <$> [C.exp| short { $(FontGlyph* ptr)->x_ } |]
    _fontGlyphY <- fromIntegral <$> [C.exp| short { $(FontGlyph* ptr)->y_ } |]
    _fontGlyphWidth <- fromIntegral <$> [C.exp| short { $(FontGlyph* ptr)->width_ } |]
    _fontGlyphHeight <- fromIntegral <$> [C.exp| short { $(FontGlyph* ptr)->height_ } |]
    _fontGlyphOffsetX <- fromIntegral <$> [C.exp| short { $(FontGlyph* ptr)->offsetX_ } |]
    _fontGlyphOffsetY <- fromIntegral <$> [C.exp| short { $(FontGlyph* ptr)->offsetY_ } |]
    _fontGlyphAdvanceX <- fromIntegral <$> [C.exp| short { $(FontGlyph* ptr)->advanceX_ } |]
    _fontGlyphPage <- fromIntegral <$> [C.exp| unsigned int { $(FontGlyph* ptr)->page_ } |]
    _fontGlyphUsed <- toBool <$> [C.exp| int { (int)$(FontGlyph* ptr)->used_ } |]
    return FontGlyph {..}
  poke ptr FontGlyph {..} = [C.block| void {
      $(FontGlyph* ptr)->x_ = $(short _fontGlyphX');
      $(FontGlyph* ptr)->y_ = $(short _fontGlyphY');
      $(FontGlyph* ptr)->width_ = $(short _fontGlyphWidth');
      $(FontGlyph* ptr)->height_ = $(short _fontGlyphHeight');
      $(FontGlyph* ptr)->offsetX_ = $(short _fontGlyphOffsetX');
      $(FontGlyph* ptr)->offsetY_ = $(short _fontGlyphOffsetY');
      $(FontGlyph* ptr)->advanceX_ = $(short _fontGlyphAdvanceX');
      $(FontGlyph* ptr)->page_ = $(unsigned int _fontGlyphPage');
      $(FontGlyph* ptr)->used_ = $(int _fontGlyphUsed') != 0;
    } |]
    where
    _fontGlyphX' = fromIntegral _fontGlyphX
    _fontGlyphY' = fromIntegral _fontGlyphY
    _fontGlyphWidth' = fromIntegral _fontGlyphWidth
    _fontGlyphHeight' = fromIntegral _fontGlyphHeight
    _fontGlyphOffsetX' = fromIntegral _fontGlyphOffsetX
    _fontGlyphOffsetY' = fromIntegral _fontGlyphOffsetY
    _fontGlyphAdvanceX' = fromIntegral _fontGlyphAdvanceX
    _fontGlyphPage' = fromIntegral _fontGlyphPage
    _fontGlyphUsed' = fromBool _fontGlyphUsed

-- | Return the kerning for a character and the next character.
-- short GetKerning(unsigned c, unsigned d) const;
fontFaceGetKerning :: (Parent FontFace a, Pointer ptr a, MonadIO m)
  => ptr -- ^ pointer to FontFace or ascentor
  -> Word -- ^ c
  -> Word -- ^ d
  -> m Word16
fontFaceGetKerning p c d = liftIO $ do
  let ptr = parentPointer p
      c' = fromIntegral c
      d' = fromIntegral d
  fromIntegral <$> [C.exp| short { $(FontFace* ptr)->GetKerning($(unsigned int c'), $(unsigned int d')) } |]

-- | Return true when one of the texture has a data loss.
-- bool IsDataLost() const;
fontFaceIsDataLost :: (Parent FontFace a, Pointer ptr a, MonadIO m)
  => ptr -- ^ pointer to FontFace or ascentor
  -> m Bool
fontFaceIsDataLost p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(FontFace* ptr)->IsDataLost() } |]

-- | Return point size.
-- int GetPointSize() const { return pointSize_; }
fontFaceGetPointSize :: (Parent FontFace a, Pointer ptr a, MonadIO m)
  => ptr -- ^ pointer to FontFace or ascentor
  -> m Int
fontFaceGetPointSize p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(FontFace* ptr)->GetPointSize() } |]

-- | Return row height.
-- int GetRowHeight() const { return rowHeight_; }
fontFaceGetRowHeight :: (Parent FontFace a, Pointer ptr a, MonadIO m)
  => ptr -- ^ pointer to FontFace or ascentor
  -> m Int
fontFaceGetRowHeight p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int{ $(FontFace* ptr)->GetRowHeight() } |]

-- | Return textures.
-- const Vector<SharedPtr<Texture2D> >& GetTextures() const { return textures_; }
fontFaceGetTextures :: (Parent FontFace a, Pointer ptr a, MonadIO m, ForeignVector v (SharedPtr Texture2D))
  => ptr -- ^ pointer to FontFace or ascentor
  -> m (v (SharedPtr Texture2D))
fontFaceGetTextures p = liftIO $ do
  let ptr = parentPointer p
  peekForeignVectorAs =<< [C.exp| const VectorSharedTexture2DPtr* { &$(FontFace* ptr)->GetTextures() } |]
