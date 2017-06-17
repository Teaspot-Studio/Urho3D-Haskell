{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.Text(
    Text
  , textContext
  , SharedText
  , TextEffect(..)
  , CharLocation(..)
  , GlyphLocation(..)
  , defaultFontSize
  , textSetFontByName
  , textSetFont
  , textSetFontSize
  , textSetText
  , textSetTextAlignment
  , textSetRowSpacing
  , textSetWordwrap
  , textSetAutoLocalizable
  , textSetSelection
  , textClearSelection
  , textSetSelectionColor
  , textSetHoverColor
  , textSetTextEffect
  , textSetEffectShadowOffset
  , textSetEffectStrokeThickness
  , textSetEffectRoundStroke
  , textSetEffectColor
  , textGetFont
  , textGetFontSize
  , textGetText
  , textGetTextAlignment
  , textGetRowSpacing
  , textGetWordwrap
  , textGetAutoLocalizable
  , textGetSelectionStart
  , textGetSelectionLength
  , textGetSelectionColor
  , textGetHoverColor
  , textGetTextEffect
  , textGetEffectShadowOffset
  , textGetEffectStrokeThickness
  , textGetEffectRoundStroke
  , textGetEffectColor
  , textGetRowHeight
  , textGetNumRows
  , textGetNumChars
  , textGetRowWidth
  , textGetCharPosition
  , textGetCharSize
  , textSetEffectDepthBias
  , textGetEffectDepthBias
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.Text as T

import Data.Monoid
import Foreign
import Foreign.C
import GHC.Generics
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Font
import Graphics.Urho3D.UI.Internal.Text
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.FontFace

C.context (C.cppCtx
  <> animatableContext
  <> colorContext
  <> contextContext
  <> fontContext
  <> fontFaceContext
  <> objectContext
  <> serializableContext
  <> sharedTextPtrCntx
  <> stringContext
  <> textCntx
  <> uiElementContext
  <> vector2Context
  )
C.include "<Urho3D/UI/Text.h>"
C.using "namespace Urho3D"

textContext :: C.Context
textContext = sharedTextPtrCntx <> textCntx

instance Creatable (Ptr Text) where
  type CreationOptions (Ptr Text) = Ptr Context

  newObject ptr = liftIO $ [C.exp| Text* { new Text( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Text* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement] ''Text

instance UIElem Text where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Text::GetTypeStatic().Value() } |]

sharedPtr "Text"

-- | Text effect
data TextEffect = TE'None | TE'Shadow | TE'Stroke
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

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

instance Storable CharLocation where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(CharLocation) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<CharLocation>::AlignmentOf } |]
  peek ptr = do
    _charLocationPosition <- peek =<< [C.exp| IntVector2* { &$(CharLocation* ptr)->position_ } |]
    _charLocationSize <- peek =<< [C.exp| IntVector2* { &$(CharLocation* ptr)->size_ } |]
    return CharLocation {..}
  poke ptr CharLocation {..} =
    with _charLocationPosition $ \_charLocationPosition' ->
    with _charLocationSize $ \_charLocationSize' ->
    [C.block| void {
      $(CharLocation* ptr)->position_ = *$(IntVector2* _charLocationPosition');
      $(CharLocation* ptr)->size_ = *$(IntVector2* _charLocationSize');
    } |]

instance Storable GlyphLocation where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(GlyphLocation) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<GlyphLocation>::AlignmentOf } |]
  peek ptr = do
    _glyphLocationX <- fromIntegral <$> [C.exp| int { $(GlyphLocation* ptr)->x_ } |]
    _glyphLocationY <- fromIntegral <$> [C.exp| int { $(GlyphLocation* ptr)->y_ } |]
    _glyphLocationGlyph <- [C.exp| const FontGlyph* { $(GlyphLocation* ptr)->glyph_ } |]
    return GlyphLocation {..}
  poke ptr GlyphLocation {..} =
    [C.block| void {
      $(GlyphLocation* ptr)->x_ = $(int _glyphLocationX');
      $(GlyphLocation* ptr)->y_ = $(int _glyphLocationY');
      $(GlyphLocation* ptr)->glyph_ = $(FontGlyph* _glyphLocationGlyph);
    } |]
    where
      _glyphLocationX' = fromIntegral _glyphLocationX
      _glyphLocationY' = fromIntegral _glyphLocationY

defaultFontSize :: Int
defaultFontSize = 12

-- | Set font by looking from resource cache by name and font size. Return true if successful.
textSetFontByName :: (Parent Text text, Pointer pText text, MonadIO m)
  => pText -- ^ Pointer to Text object
  -> String -- ^ Font name
  -> Int -- ^ Font size
  -> m Bool
textSetFontByName pText name fsize = liftIO $ withCString name $ \name' -> do
  let ptrText = parentPointer pText
      fsize' = fromIntegral fsize
  toBool <$> [C.exp| int { (int)$(Text* ptrText)->SetFont(String($(const char* name')), $(int fsize')) } |]

-- | Set font and font size. Return true if successful.
textSetFont :: (Parent Text text, Pointer pText text, Parent Font font, Pointer pFont font, MonadIO m)
  => pText -- ^ Pointer to Text object
  -> pFont -- ^ Pointer to Font object
  -> Int -- ^ Font size
  -> m Bool
textSetFont pText pFont fsize = liftIO $ do
  let ptrText = parentPointer pText
      ptrFont = parentPointer pFont
      fsize' = fromIntegral fsize
  toBool <$> [C.exp| int { (int)$(Text* ptrText)->SetFont($(Font* ptrFont), $(int fsize')) } |]

-- | Set font by looking from resource cache by name and font size. Return true if successful.
textSetFontSize :: (Parent Text text, Pointer pText text, MonadIO m)
  => pText -- ^ Pointer to Text object
  -> Int -- ^ Font size
  -> m Bool
textSetFontSize pText fsize = liftIO $ do
  let ptrText = parentPointer pText
      fsize' = fromIntegral fsize
  toBool <$> [C.exp| int { (int)$(Text* ptrText)->SetFontSize($(int fsize')) } |]

-- | Set text. Text is assumed to be either ASCII or UTF8-encoded.
textSetText :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> T.Text -- ^ Text contents
  -> m ()
textSetText p str = liftIO $ textAsPtrW32 str $ \str' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Text* ptr)->SetText(String($(wchar_t* str'))) } |]

-- | Set row alignment.
textSetTextAlignment :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> HorizontalAlignment -- ^ aligment
  -> m ()
textSetTextAlignment p align = liftIO $ do
  let ptr = parentPointer p
      align' = fromIntegral . fromEnum $ align
  [C.exp| void { $(Text* ptr)->SetTextAlignment((HorizontalAlignment)$(int align')) } |]

-- | Set row spacing, 1.0 for original font spacing.
textSetRowSpacing :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Float -- ^ spacing
  -> m ()
textSetRowSpacing p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(Text* ptr)->SetRowSpacing($(float v')) } |]

-- | Set wordwrap. In wordwrap mode the text element will respect its current width. Otherwise it resizes itself freely.
textSetWordwrap :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Bool -- ^ enable
  -> m ()
textSetWordwrap p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(Text* ptr)->SetWordwrap($(int b') != 0) } |]

-- | The text will be automatically translated. The text value used as string identifier.
textSetAutoLocalizable :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Bool -- ^ enable
  -> m ()
textSetAutoLocalizable p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(Text* ptr)->SetAutoLocalizable($(int b') != 0) } |]

-- | Set selection. When length is not provided, select until the text ends.
textSetSelection :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Word -- ^ start
  -> Word -- ^ length (default max unsigned)
  -> m ()
textSetSelection p start l = liftIO $ do
  let ptr = parentPointer p
      start' = fromIntegral start
      l' = fromIntegral l
  [C.exp| void { $(Text* ptr)->SetSelection($(unsigned int start'), $(unsigned int l')) } |]

-- | Clear selection.
textClearSelection :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m ()
textClearSelection p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Text* ptr)->ClearSelection() } |]

-- | Set selection background color. Color with 0 alpha (default) disables.
textSetSelectionColor :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Color -- ^ color
  -> m ()
textSetSelectionColor p color = liftIO $ with color $ \color' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Text* ptr)->SetSelectionColor(*$(Color* color')) } |]

-- | Set hover background color. Color with 0 alpha (default) disables.
textSetHoverColor :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Color -- ^ color
  -> m ()
textSetHoverColor p color = liftIO $ with color $ \color' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Text* ptr)->SetHoverColor(*$(Color* color')) } |]

-- | Set text effect.
textSetTextEffect :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> TextEffect -- ^ color
  -> m ()
textSetTextEffect p effect = liftIO $ do
  let ptr = parentPointer p
      effect' = fromIntegral . fromEnum $ effect
  [C.exp| void { $(Text* ptr)->SetTextEffect((TextEffect)$(int effect')) } |]

-- | Set shadow offset.
textSetEffectShadowOffset :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> IntVector2 -- ^ offset
  -> m ()
textSetEffectShadowOffset p offset = liftIO $ with offset $ \offset' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Text* ptr)->SetEffectShadowOffset(*$(IntVector2* offset')) } |]

-- | Set stroke thickness.
textSetEffectStrokeThickness :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Int -- ^ thickness
  -> m ()
textSetEffectStrokeThickness p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(Text* ptr)->SetEffectStrokeThickness($(int v')) } |]

-- | Set stroke rounding. Corners of the font will be rounded off in the stroke so the stroke won't have corners.
textSetEffectRoundStroke :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Bool -- ^ round stroke
  -> m ()
textSetEffectRoundStroke p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(Text* ptr)->SetEffectRoundStroke($(int v') != 0) } |]

-- | Set effect color.
textSetEffectColor :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Color -- ^ effect color
  -> m ()
textSetEffectColor p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Text* ptr)->SetEffectColor(*$(Color* v')) } |]

-- | Return font.
textGetFont :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m (Ptr Font)
textGetFont p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Font* { $(Text* ptr)->GetFont() } |]

-- | Return font size.
textGetFontSize :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Int
textGetFontSize p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(Text* ptr)->GetFontSize() } |]

-- | Return text.
textGetText :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m T.Text
textGetText p = liftIO $ do
  let ptr = parentPointer p
  loadConstUrhoText =<< [C.exp| const String* { &$(Text* ptr)->GetText() } |]

-- | Return row alignment.
textGetTextAlignment :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m HorizontalAlignment
textGetTextAlignment p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral<$> [C.exp| int { (int)$(Text* ptr)->GetTextAlignment() } |]

-- | Return row spacing.
textGetRowSpacing :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Float
textGetRowSpacing p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Text* ptr)->GetRowSpacing() } |]

-- | Return wordwrap mode.
textGetWordwrap :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Bool
textGetWordwrap p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Text* ptr)->GetWordwrap() } |]

-- | Return auto localizable mode.
textGetAutoLocalizable :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Bool
textGetAutoLocalizable p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Text* ptr)->GetAutoLocalizable() } |]

-- | Return selection start.
textGetSelectionStart :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Word
textGetSelectionStart p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Text* ptr)->GetSelectionStart() } |]

-- | Return selection length.
textGetSelectionLength :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Word
textGetSelectionLength p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Text* ptr)->GetSelectionLength() } |]

-- | Return selection background color.
textGetSelectionColor :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Color
textGetSelectionColor p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Color* { &$(Text* ptr)->GetSelectionColor() } |]

-- | Return hover background color.
textGetHoverColor :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Color
textGetHoverColor p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Color* { &$(Text* ptr)->GetHoverColor() } |]

-- | Return text effect.
textGetTextEffect :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m TextEffect
textGetTextEffect p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int { (int)$(Text* ptr)->GetTextEffect() } |]

-- | Return effect shadow offset.
textGetEffectShadowOffset :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m IntVector2
textGetEffectShadowOffset p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* { &$(Text* ptr)->GetEffectShadowOffset() } |]

-- | Return effect stroke thickness.
textGetEffectStrokeThickness :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Int
textGetEffectStrokeThickness p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(Text* ptr)->GetEffectStrokeThickness() } |]

-- | Return effect round stroke.
textGetEffectRoundStroke :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Bool
textGetEffectRoundStroke p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(Text* ptr)->GetEffectRoundStroke() } |]

-- | Return effect color.
textGetEffectColor :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Color
textGetEffectColor p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Color* { &$(Text* ptr)->GetEffectColor() } |]

-- | Return row height.
textGetRowHeight :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Int
textGetRowHeight p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(Text* ptr)->GetRowHeight() } |]

-- | Return row height.
textGetNumRows :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Word
textGetNumRows p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Text* ptr)->GetNumRows() } |]

-- | Return number of characters.
textGetNumChars :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Word
textGetNumChars p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Text* ptr)->GetNumChars() } |]

-- | Return width of row by index.
textGetRowWidth :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Word -- ^ index
  -> m Int
textGetRowWidth p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  fromIntegral <$> [C.exp| int { $(Text* ptr)->GetRowWidth($(unsigned int i')) } |]

-- | Return position of character by index relative to the text element origin.
textGetCharPosition :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Word -- ^ index
  -> m IntVector2
textGetCharPosition p i = liftIO $ alloca $ \resptr -> do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.block| void {
    *$(IntVector2* resptr) = $(Text* ptr)->GetCharPosition($(unsigned int i'));
  } |]
  peek resptr

-- | Return size of character by index.
textGetCharSize :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Word -- ^ index
  -> m IntVector2
textGetCharSize p i = liftIO $ alloca $ \resptr -> do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.block| void {
    *$(IntVector2* resptr) = $(Text* ptr)->GetCharSize($(unsigned int i'));
  } |]
  peek resptr

-- | Set text effect Z bias. Zero by default, adjusted only in 3D mode.
textSetEffectDepthBias :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> Float -- ^ bias
  -> m ()
textSetEffectDepthBias p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(Text* ptr)->SetEffectDepthBias($(float v')) } |]

-- | Return effect Z bias.
textGetEffectDepthBias :: (Parent Text a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to Text object
  -> m Float
textGetEffectDepthBias p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(Text* ptr)->GetEffectDepthBias() } |]

-- /// Set font attribute.
-- void SetFontAttr(const ResourceRef& value);
-- /// Return font attribute.
-- ResourceRef GetFontAttr() const;
-- /// Set text attribute.
-- void SetTextAttr(const String& value);
-- /// Return text attribute.
-- String GetTextAttr() const;
