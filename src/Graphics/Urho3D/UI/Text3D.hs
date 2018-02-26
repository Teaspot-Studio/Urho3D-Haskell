{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.Text3D(
    Text3D
  , text3DContext
  , SharedText3D
  , defaultFontSize
  , Text3DSetFont(..)
  , text3DSetFontSize
  , text3DSetMaterial
  , text3DSetText
  , text3DSetAlignment
  , text3DSetHorizontalAlignment
  , text3DSetVerticalAlignment
  , text3DSetTextAlignment
  , text3DSetRowSpacing
  , text3DSetWordwrap
  , text3DSetTextEffect
  , text3DSetEffectShadowOffset
  , text3DSetEffectStrokeThickness
  , text3DSetEffectRoundStroke
  , text3DSetEffectColor
  , text3DSetEffectDepthBias
  , text3DSetWidth
  , text3DSetColor
  , text3DSetCornerColor
  , text3DSetOpacity
  , text3DSetFixedScreenSize
  , text3DSetFaceCameraMode
  , text3DGetFont
  , text3DGetFontSize
  , text3DGetMaterial
  , text3DGetText
  , text3DGetTextAlignment
  , text3DGetHorizontalAlignment
  , text3DGetVerticalAlignment
  , text3DGetRowSpacing
  , text3DGetWordwrap
  , text3DGetTextEffect
  , text3DGetEffectShadowOffset
  , text3DGetEffectStrokeThickness
  , text3DGetEffectRoundStroke
  , text3DGetEffectColor
  , text3DGetEffectDepthBias
  , text3DGetWidth
  , text3DGetHeight
  , text3DGetRowHeight
  , text3DGetNumRows
  , text3DGetNumChars
  , text3DGetRowWidth
  , text3DGetCharPosition
  , text3DGetCharSize
  , text3DGetColor
  , text3DGetOpacity
  , text3DIsFixedScreenSize
  , text3DGetFaceCameraMode
  , text3DSetFontAttr
  , text3DGetFontAttr
  , text3DSetMaterialAttr
  , text3DGetMaterialAttr
  , text3DSetTextAttr
  , text3DGetTextAttr
  , text3DGetColorAttr
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.Text as T

import Data.Monoid
import Foreign
import Foreign.C.String
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.Text3D
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Defs
import Graphics.Urho3D.Graphics.Drawable
import Graphics.Urho3D.Graphics.Material
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.Font
import Graphics.Urho3D.UI.Text

C.context (C.cppCtx
  <> sharedText3DPtrCntx
  <> text3DCntx
  <> animatableContext
  <> colorContext
  <> componentContext
  <> contextContext
  <> drawableContext
  <> fontContext
  <> materialContext
  <> objectContext
  <> serializableContext
  <> stringContext
  <> uiElementContext
  <> variantContext
  <> vector2Context
  <> vector2Context
  )
C.include "<Urho3D/UI/Text3D.h>"
C.using "namespace Urho3D"

text3DContext :: C.Context
text3DContext = sharedText3DPtrCntx
  <> text3DCntx

instance Creatable (Ptr Text3D) where
  type CreationOptions (Ptr Text3D) = Ptr Context

  newObject ptr = liftIO $ [C.exp| Text3D* { new Text3D( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Text3D* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Drawable] ''Text3D

instance UIElem Text3D where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Text3D::GetTypeStatic().Value() } |]

sharedPtr "Text3D"

class Text3DSetFont a where
  -- | Set font by looking from resource cache by name and font size. Return true if successful.
  text3DSetFont :: (Parent Text3D a, Pointer ptr a, MonadIO m)
    => ptr -- ^ Pointer to text3D or ascentor
    -> a -- ^ Font name or font pointer
    -> Int -- ^ size (default is defaultFontSize)
    -> m Bool

-- | bool SetFont(const String& fontName, int size = DEFAULT_FONT_SIZE);
instance Text3DSetFont String where
  text3DSetFont p fontName s = liftIO $ withCString fontName $ \fontName' -> do
    let ptr = parentPointer p
        s' = fromIntegral s
    toBool <$> [C.exp| int {(int)$(Text3D* ptr)->SetFont(String($(const char* fontName')), $(int s'))} |]
  {-# INLINE text3DSetFont #-}

-- | bool SetFont(Font* font, int size = DEFAULT_FONT_SIZE);
instance Text3DSetFont (Ptr Font) where
  text3DSetFont p font s = liftIO $  do
    let ptr = parentPointer p
        s' = fromIntegral s
    toBool <$> [C.exp| int {(int)$(Text3D* ptr)->SetFont($(Font* font), $(int s'))} |]
  {-# INLINE text3DSetFont #-}

-- | Set font size only while retaining the existing font. Return true if successful.
-- bool SetFontSize(int size);
text3DSetFontSize :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Int -- ^ size
  -> m Bool
text3DSetFontSize p s = liftIO $  do
  let ptr = parentPointer p
      s' = fromIntegral s
  toBool <$> [C.exp| int {(int)$(Text3D* ptr)->SetFontSize($(int s'))} |]

-- | Set material.
-- void SetMaterial(Material* material);
text3DSetMaterial :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Ptr Material -- ^ material
  -> m ()
text3DSetMaterial p mat = liftIO $  do
  let ptr = parentPointer p
  [C.exp| void {$(Text3D* ptr)->SetMaterial($(Material* mat))} |]

-- | Set text. Text is assumed to be either ASCII or UTF8-encoded.
-- void SetText(const String& text);
text3DSetText :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> T.Text
  -> m ()
text3DSetText p str = liftIO $ textAsPtrW32 str $ \str' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Text3D* ptr)->SetText(String($(wchar_t* str')))} |]

-- | Set horizontal and vertical alignment.
-- void SetAlignment(HorizontalAlignment hAlign, VerticalAlignment vAlign);
text3DSetAlignment :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> HorizontalAlignment
  -> VerticalAlignment
  -> m ()
text3DSetAlignment p hAlign vAlign = liftIO $  do
  let ptr = parentPointer p
      hAlign' = fromIntegral . fromEnum $ hAlign
      vAlign' = fromIntegral . fromEnum $ vAlign
  [C.exp| void {$(Text3D* ptr)->SetAlignment((HorizontalAlignment)$(int hAlign'), (VerticalAlignment)$(int vAlign'))} |]

-- | Set horizontal alignment.
-- void SetHorizontalAlignment(HorizontalAlignment align);
text3DSetHorizontalAlignment :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> HorizontalAlignment
  -> m ()
text3DSetHorizontalAlignment p hAlign = liftIO $  do
  let ptr = parentPointer p
      hAlign' = fromIntegral . fromEnum $ hAlign
  [C.exp| void {$(Text3D* ptr)->SetHorizontalAlignment((HorizontalAlignment)$(int hAlign'))} |]

-- | Set vertical alignment.
-- void SetVerticalAlignment(VerticalAlignment align);
text3DSetVerticalAlignment :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> VerticalAlignment
  -> m ()
text3DSetVerticalAlignment p vAlign = liftIO $  do
  let ptr = parentPointer p
      vAlign' = fromIntegral . fromEnum $ vAlign
  [C.exp| void {$(Text3D* ptr)->SetVerticalAlignment((VerticalAlignment)$(int vAlign'))} |]

-- | Set row alignment.
-- void SetTextAlignment(HorizontalAlignment align);
text3DSetTextAlignment :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> HorizontalAlignment
  -> m ()
text3DSetTextAlignment p hAlign = liftIO $  do
  let ptr = parentPointer p
      hAlign' = fromIntegral . fromEnum $ hAlign
  [C.exp| void {$(Text3D* ptr)->SetTextAlignment((HorizontalAlignment)$(int hAlign'))} |]

-- | Set row spacing, 1.0 for original font spacing.
-- void SetRowSpacing(float spacing);
text3DSetRowSpacing :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Float -- ^ spacing
  -> m ()
text3DSetRowSpacing p v = liftIO $  do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Text3D* ptr)->SetRowSpacing($(float v'))} |]

-- | Set wordwrap. In wordwrap mode the text element will respect its current width. Otherwise it resizes itself freely.
-- void SetWordwrap(bool enable);
text3DSetWordwrap :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Bool -- ^ enable
  -> m ()
text3DSetWordwrap p v = liftIO $  do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(Text3D* ptr)->SetWordwrap($(int v') != 0)} |]

-- | Set text effect.
-- void SetTextEffect(TextEffect textEffect);
text3DSetTextEffect :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> TextEffect
  -> m ()
text3DSetTextEffect p e = liftIO $  do
  let ptr = parentPointer p
      e' = fromIntegral . fromEnum $ e
  [C.exp| void {$(Text3D* ptr)->SetTextEffect((TextEffect)$(int e'))} |]

-- | Set shadow offset.
-- void SetEffectShadowOffset(const IntVector2& offset);
text3DSetEffectShadowOffset :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> IntVector2 -- ^ offset
  -> m ()
text3DSetEffectShadowOffset p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Text3D* ptr)->SetEffectShadowOffset(*$(IntVector2* v'))} |]

-- | Set stroke thickness.
-- void SetEffectStrokeThickness(int thickness);
text3DSetEffectStrokeThickness :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Int -- ^ thickness
  -> m ()
text3DSetEffectStrokeThickness p v = liftIO $  do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void {$(Text3D* ptr)->SetEffectStrokeThickness($(int v'))} |]

-- | Set stroke rounding. Corners of the font will be rounded off in the stroke so the stroke won't have corners.
-- void SetEffectRoundStroke(bool roundStroke);
text3DSetEffectRoundStroke :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Bool -- ^ round stroke
  -> m ()
text3DSetEffectRoundStroke p v = liftIO $  do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(Text3D* ptr)->SetEffectRoundStroke($(int v')!=0)} |]

-- | Set effect color.
-- void SetEffectColor(const Color& effectColor);
text3DSetEffectColor :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Color -- ^ effect color
  -> m ()
text3DSetEffectColor p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Text3D* ptr)->SetEffectColor(*$(Color* v'))} |]

-- | Set effect Z bias.
-- void SetEffectDepthBias(float bias);
text3DSetEffectDepthBias :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Float -- ^ bias
  -> m ()
text3DSetEffectDepthBias p v = liftIO $  do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Text3D* ptr)->SetEffectDepthBias($(float v'))} |]

-- | Set text width. Only has effect in word wrap mode.
-- void SetWidth(int width);
text3DSetWidth :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Int -- ^ width
  -> m ()
text3DSetWidth p v = liftIO $  do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void {$(Text3D* ptr)->SetWidth($(int v'))} |]

-- | Set color on all corners.
-- void SetColor(const Color& color);
text3DSetColor :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Color -- ^ color
  -> m ()
text3DSetColor p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Text3D* ptr)->SetColor(*$(Color* v'))} |]

-- | Set color on one corner.
-- void SetColor(Corner corner, const Color& color);
text3DSetCornerColor :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Corner
  -> Color
  -> m ()
text3DSetCornerColor p cor col = liftIO $ with col $ \col' -> do
  let ptr = parentPointer p
      cor' = fromIntegral . fromEnum $ cor
  [C.exp| void {$(Text3D* ptr)->SetColor((Corner)$(int cor'), *$(Color* col'))} |]

-- | Set opacity.
-- void SetOpacity(float opacity);
text3DSetOpacity :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Float
  -> m ()
text3DSetOpacity p v = liftIO $  do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(Text3D* ptr)->SetOpacity($(float v'))} |]

-- | Set whether text has fixed size on screen (pixel-perfect) regardless of distance to camera. Works best when combined with face camera rotation. Default false.
-- void SetFixedScreenSize(bool enable);
text3DSetFixedScreenSize :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Bool -- ^ enable
  -> m ()
text3DSetFixedScreenSize p v = liftIO $  do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(Text3D* ptr)->SetFixedScreenSize($(int v') != 0)} |]

-- | Set how the text should rotate in relation to the camera. Default is to not rotate (FC_NONE.)
-- void SetFaceCameraMode(FaceCameraMode mode);
text3DSetFaceCameraMode :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> FaceCameraMode
  -> m ()
text3DSetFaceCameraMode p m = liftIO $  do
  let ptr = parentPointer p
      m' = fromIntegral . fromEnum $ m
  [C.exp| void {$(Text3D* ptr)->SetFaceCameraMode((FaceCameraMode)$(int m'))} |]

-- | Return font.
-- Font* GetFont() const;
text3DGetFont :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m (Ptr Font)
text3DGetFont p = liftIO $  do
  let ptr = parentPointer p
  [C.exp| Font* {$(Text3D* ptr)->GetFont()} |]

-- | Return font size.
-- int GetFontSize() const;
text3DGetFontSize :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Int
text3DGetFontSize p = liftIO $  do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int {$(Text3D* ptr)->GetFontSize()} |]

-- | Return material.
-- Material* GetMaterial() const;
text3DGetMaterial :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m (Ptr Material)
text3DGetMaterial p = liftIO $  do
  let ptr = parentPointer p
  [C.exp| Material* {$(Text3D* ptr)->GetMaterial()} |]

-- | Return text.
-- const String& GetText() const;
text3DGetText :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m T.Text
text3DGetText p = liftIO $  do
  let ptr = parentPointer p
  loadConstUrhoText =<< [C.exp| const String* { &$(Text3D* ptr)->GetText()} |]

-- | Return row alignment.
-- HorizontalAlignment GetTextAlignment() const;
text3DGetTextAlignment :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m HorizontalAlignment
text3DGetTextAlignment p = liftIO $  do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(Text3D* ptr)->GetTextAlignment()} |]

-- | Return horizontal alignment.
-- HorizontalAlignment GetHorizontalAlignment() const;
text3DGetHorizontalAlignment :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m HorizontalAlignment
text3DGetHorizontalAlignment p = liftIO $  do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(Text3D* ptr)->GetHorizontalAlignment()} |]

-- | Return vertical alignment.
-- VerticalAlignment GetVerticalAlignment() const;
text3DGetVerticalAlignment :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m VerticalAlignment
text3DGetVerticalAlignment p = liftIO $  do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(Text3D* ptr)->GetVerticalAlignment()} |]

-- | Return row spacing.
-- float GetRowSpacing() const;
text3DGetRowSpacing :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Float
text3DGetRowSpacing p = liftIO $  do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Text3D* ptr)->GetRowSpacing()} |]

-- | Return wordwrap mode.
-- bool GetWordwrap() const;
text3DGetWordwrap :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Bool
text3DGetWordwrap p = liftIO $  do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Text3D* ptr)->GetWordwrap()} |]

-- | Return text effect.
-- TextEffect GetTextEffect() const;
text3DGetTextEffect :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m TextEffect
text3DGetTextEffect p = liftIO $  do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(Text3D* ptr)->GetTextEffect()} |]

-- | Return effect shadow offset.
-- const IntVector2& GetEffectShadowOffset() const;
text3DGetEffectShadowOffset :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m IntVector2
text3DGetEffectShadowOffset p = liftIO $  do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* {&$(Text3D* ptr)->GetEffectShadowOffset()} |]

-- | Return effect stroke thickness.
-- int GetEffectStrokeThickness() const;
text3DGetEffectStrokeThickness :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Int
text3DGetEffectStrokeThickness p = liftIO $  do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int {$(Text3D* ptr)->GetEffectStrokeThickness()} |]

-- | Return effect round stroke.
-- bool GetEffectRoundStroke() const;
text3DGetEffectRoundStroke :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Bool
text3DGetEffectRoundStroke p = liftIO $  do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Text3D* ptr)->GetEffectRoundStroke()} |]

-- | Return effect color.
-- const Color& GetEffectColor() const;
text3DGetEffectColor :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Color
text3DGetEffectColor p = liftIO $  do
  let ptr = parentPointer p
  peek =<< [C.exp| const Color* {&$(Text3D* ptr)->GetEffectColor()} |]

-- | Return effect depth bias.
-- float GetEffectDepthBias() const;
text3DGetEffectDepthBias :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Float
text3DGetEffectDepthBias p = liftIO $  do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Text3D* ptr)->GetEffectDepthBias()} |]

-- | Return text width.
-- int GetWidth() const;
text3DGetWidth :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Int
text3DGetWidth p = liftIO $  do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int {$(Text3D* ptr)->GetWidth()} |]

-- | Return text height.
-- int GetHeight() const;
text3DGetHeight :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Int
text3DGetHeight p = liftIO $  do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int {$(Text3D* ptr)->GetHeight()} |]

-- | Return row height.
-- int GetRowHeight() const;
text3DGetRowHeight :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Int
text3DGetRowHeight p = liftIO $  do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int {$(Text3D* ptr)->GetRowHeight()} |]

-- | Return number of rows.
-- unsigned GetNumRows() const;
text3DGetNumRows :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Word
text3DGetNumRows p = liftIO $  do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Text3D* ptr)->GetNumRows()} |]

-- | Return number of characters.
-- unsigned GetNumChars() const;
text3DGetNumChars :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Word
text3DGetNumChars p = liftIO $  do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(Text3D* ptr)->GetNumChars()} |]

-- | Return width of row by index.
-- int GetRowWidth(unsigned index) const;
text3DGetRowWidth :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Word -- ^ index
  -> m Int
text3DGetRowWidth p i = liftIO $  do
  let ptr = parentPointer p
      i' = fromIntegral i
  fromIntegral <$> [C.exp| int {$(Text3D* ptr)->GetRowWidth($(unsigned int i'))} |]

-- | Return position of character by index relative to the text element origin.
-- Vector2 GetCharPosition(unsigned index);
text3DGetCharPosition :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Word -- ^ index
  -> m Vector2
text3DGetCharPosition p i = liftIO $ alloca $ \vptr -> do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void {*$(Vector2* vptr) = $(Text3D* ptr)->GetCharPosition($(unsigned int i'))} |]
  peek vptr

-- | Return size of character by index.
-- IntVector2 GetCharSize(unsigned index);
text3DGetCharSize :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Word -- ^ index
  -> m Vector2
text3DGetCharSize p i = liftIO $ alloca $ \vptr -> do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void {*$(Vector2* vptr) = $(Text3D* ptr)->GetCharSize($(unsigned int i'))} |]
  peek vptr

-- | Return corner color.
-- const Color& GetColor(Corner corner) const;
text3DGetColor :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> Corner
  -> m Color
text3DGetColor p cor = liftIO $  do
  let ptr = parentPointer p
      cor' = fromIntegral . fromEnum $ cor
  peek =<< [C.exp| const Color* {&$(Text3D* ptr)->GetColor((Corner)$(int cor'))} |]

-- | Return opacity.
-- float GetOpacity() const;
text3DGetOpacity :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Float
text3DGetOpacity p = liftIO $  do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(Text3D* ptr)->GetOpacity()} |]

-- | Return whether text has fixed screen size.
-- bool IsFixedScreenSize() const { return fixedScreenSize_; }
text3DIsFixedScreenSize :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Bool
text3DIsFixedScreenSize p = liftIO $  do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(Text3D* ptr)->IsFixedScreenSize()} |]

-- | Return how the text rotates in relation to the camera.
-- FaceCameraMode GetFaceCameraMode() const { return faceCameraMode_; }
text3DGetFaceCameraMode :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m FaceCameraMode
text3DGetFaceCameraMode p = liftIO $  do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(Text3D* ptr)->GetFaceCameraMode()} |]

-- | Set font attribute.
-- void SetFontAttr(const ResourceRef& value);
text3DSetFontAttr :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> ResourceRef -- ^ value
  -> m ()
text3DSetFontAttr p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Text3D* ptr)->SetFontAttr(*$(ResourceRef* v'))} |]

-- | Return font attribute.
-- ResourceRef GetFontAttr() const;
text3DGetFontAttr :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m ResourceRef
text3DGetFontAttr p = liftIO $ alloca $ \vptr -> do
  let ptr = parentPointer p
  [C.exp| void {*$(ResourceRef* vptr) = $(Text3D* ptr)->GetFontAttr()} |]
  peek vptr

-- | Set material attribute.
-- void SetMaterialAttr(const ResourceRef& value);
text3DSetMaterialAttr :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> ResourceRef -- ^ value
  -> m ()
text3DSetMaterialAttr p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Text3D* ptr)->SetMaterialAttr(*$(ResourceRef* v'))} |]

-- | Return material attribute.
-- ResourceRef GetMaterialAttr() const;
text3DGetMaterialAttr :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m ResourceRef
text3DGetMaterialAttr p = liftIO $ alloca $ \vptr -> do
  let ptr = parentPointer p
  [C.exp| void {*$(ResourceRef* vptr) = $(Text3D* ptr)->GetMaterialAttr()} |]
  peek vptr

-- | Set text attribute.
-- void SetTextAttr(const String& value);
text3DSetTextAttr :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> String -- ^ value
  -> m ()
text3DSetTextAttr p v = liftIO $ withCString v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(Text3D* ptr)->SetTextAttr(String($(const char* v')))} |]

-- | Return text attribute.
-- String GetTextAttr() const;
text3DGetTextAttr :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m String
text3DGetTextAttr p = liftIO $  do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* {$(Text3D* ptr)->GetTextAttr().CString()} |]

-- | Get color attribute. Uses just the top-left color.
-- const Color& GetColorAttr() const { return text_.color_[0]; }
text3DGetColorAttr :: (Parent Text3D a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to text3D or ascentor
  -> m Color
text3DGetColorAttr p = liftIO $  do
  let ptr = parentPointer p
  peek =<< [C.exp| const Color* {&$(Text3D* ptr)->GetColorAttr()} |]
