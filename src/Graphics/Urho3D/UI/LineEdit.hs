{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.LineEdit(
    LineEdit
  , lineEditContext
  , SharedLineEdit
  , lineEditSetText
  , lineEditSetCursorPosition
  , lineEditSetCursorBlinkRate
  , lineEditSetMaxLength
  , lineEditSetEchoCharacter
  , lineEditSetCursorMovable
  , lineEditSetTextSelectable
  , lineEditSetTextCopyable
  , lineEditGetText
  , lineEditGetCursorPosition
  , lineEditGetCursorBlinkRate
  , lineEditGetMaxLength
  , lineEditGetEchoCharacter
  , lineEditIsCursorMovable
  , lineEditIsTextSelectable
  , lineEditIsTextCopyable
  , lineEditGetTextElement
  , lineEditGetCursor
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Foreign.C.String
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.LineEdit
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.Text

C.context (C.cppCtx
  <> sharedLineEditPtrCntx
  <> lineEditCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> textContext
  )
C.include "<Urho3D/UI/LineEdit.h>"
C.using "namespace Urho3D"

lineEditContext :: C.Context
lineEditContext = sharedLineEditPtrCntx
  <> lineEditCntx

instance Creatable (Ptr LineEdit) where
  type CreationOptions (Ptr LineEdit) = Ptr Context

  newObject ptr = liftIO $ [C.exp| LineEdit* { new LineEdit( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(LineEdit* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''LineEdit

instance UIElem LineEdit where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { LineEdit::GetTypeStatic().Value() } |]

sharedPtr "LineEdit"

-- | Set text.
-- void SetText(const String& text);
lineEditSetText :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> String
  -> m ()
lineEditSetText p str = liftIO $ withCString str $ \str' -> do
  let ptr = parentPointer p
  [C.exp| void { $(LineEdit* ptr)->SetText(String($(const char* str'))) } |]

-- | Set cursor position.
-- void SetCursorPosition(unsigned position);
lineEditSetCursorPosition :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> Word -- ^ position
  -> m ()
lineEditSetCursorPosition p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(LineEdit* ptr)->SetCursorPosition($(unsigned int v')) } |]

-- | Set cursor blink rate. 0 disables blinking.
-- void SetCursorBlinkRate(float rate);
lineEditSetCursorBlinkRate :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> Float -- ^ rate
  -> m ()
lineEditSetCursorBlinkRate p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(LineEdit* ptr)->SetCursorBlinkRate($(float v')) } |]

-- | Set maximum text length. 0 for unlimited.
-- void SetMaxLength(unsigned length);
lineEditSetMaxLength :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> Word -- ^ length
  -> m ()
lineEditSetMaxLength p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(LineEdit* ptr)->SetMaxLength($(unsigned int v')) } |]

-- | Set echo character for password entry and such. 0 (default) shows the actual text.
-- void SetEchoCharacter(unsigned c);
lineEditSetEchoCharacter :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> Word -- ^ character
  -> m ()
lineEditSetEchoCharacter p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void { $(LineEdit* ptr)->SetEchoCharacter($(unsigned int v')) } |]

-- | Set whether can move cursor with arrows or mouse, default true.
-- void SetCursorMovable(bool enable);
lineEditSetCursorMovable :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> Bool -- ^ enable
  -> m ()
lineEditSetCursorMovable p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(LineEdit* ptr)->SetCursorMovable($(int v') != 0) } |]

-- | Set whether selections are allowed, default true.
-- void SetTextSelectable(bool enable);
lineEditSetTextSelectable :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> Bool -- ^ enable
  -> m ()
lineEditSetTextSelectable p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(LineEdit* ptr)->SetTextSelectable($(int v') != 0) } |]

-- | Set whether copy-paste operations are allowed, default true.
-- void SetTextCopyable(bool enable);
lineEditSetTextCopyable :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> Bool -- ^ enable
  -> m ()
lineEditSetTextCopyable p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(LineEdit* ptr)->SetTextCopyable($(int v') != 0) } |]

-- | Return text.
-- const String& GetText() const { return line_; }
lineEditGetText :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m String
lineEditGetText p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* { $(LineEdit* ptr)->GetText().CString() } |]

-- | Return cursor position.
-- unsigned GetCursorPosition() const { return cursorPosition_; }
lineEditGetCursorPosition :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m Word
lineEditGetCursorPosition p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(LineEdit* ptr)->GetCursorPosition() } |]

-- | Return cursor blink rate.
-- float GetCursorBlinkRate() const { return cursorBlinkRate_; }
lineEditGetCursorBlinkRate :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m Float
lineEditGetCursorBlinkRate p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(LineEdit* ptr)->GetCursorBlinkRate() } |]

-- | Return maximum text length.
-- unsigned GetMaxLength() const { return maxLength_; }
lineEditGetMaxLength :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m Word
lineEditGetMaxLength p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(LineEdit* ptr)->GetMaxLength() } |]

-- | Return echo character.
-- unsigned GetEchoCharacter() const { return echoCharacter_; }
lineEditGetEchoCharacter :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m Word
lineEditGetEchoCharacter p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(LineEdit* ptr)->GetEchoCharacter() } |]

-- | Return whether can move cursor with arrows or mouse.
-- bool IsCursorMovable() const { return cursorMovable_; }
lineEditIsCursorMovable :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m Bool
lineEditIsCursorMovable p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(LineEdit* ptr)->IsCursorMovable() } |]

-- | Return whether selections are allowed.
-- bool IsTextSelectable() const { return textSelectable_; }
lineEditIsTextSelectable :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m Bool
lineEditIsTextSelectable p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(LineEdit* ptr)->IsTextSelectable() } |]

-- | Return whether copy-paste operations are allowed.
-- bool IsTextCopyable() const { return textCopyable_; }
lineEditIsTextCopyable :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m Bool
lineEditIsTextCopyable p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(LineEdit* ptr)->IsTextCopyable() } |]

-- | Return text element.
-- Text* GetTextElement() const { return text_; }
lineEditGetTextElement :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m (Ptr Text)
lineEditGetTextElement p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Text* { $(LineEdit* ptr)->GetTextElement() } |]

-- | Return cursor element.
-- BorderImage* GetCursor() const { return cursor_; }
lineEditGetCursor :: (Parent LineEdit a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to Text or ascentor
  -> m (Ptr BorderImage)
lineEditGetCursor p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| BorderImage* { $(LineEdit* ptr)->GetCursor() } |]
