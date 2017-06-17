{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Text(
    Text
  , textContext
  , SharedText
  , textSetText
  , defaultFontSize
  , textSetFont
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.Text as T

import Graphics.Urho3D.UI.Internal.Text
import Graphics.Urho3D.UI.Font
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx <> sharedTextPtrCntx <> textCntx <> contextContext <> uiElementContext <> fontContext <> animatableContext <> serializableContext <> objectContext)
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

-- | Sets Text contents
textSetText :: (Parent Text a, Pointer p a, MonadIO m) => p -- ^ Pointer to Text object
  -> T.Text -- ^ Text contents
  -> m ()
textSetText p str = liftIO $ textAsPtrW32 str $ \str' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Text* ptr)->SetText(String($(wchar_t* str'))) } |]

defaultFontSize :: Int
defaultFontSize = 12

textSetFont :: (Parent Text text, Pointer pText text, Parent Font font, Pointer pFont font, MonadIO m)
  => pText -- ^ Pointer to Text object
  -> pFont -- ^ Pointer to Font object
  -> Int -- ^ Font size
  -> m ()
textSetFont pText pFont fsize = liftIO $ do
  let ptrText = parentPointer pText
      ptrFont = parentPointer pFont
      fsize' = fromIntegral fsize
  [C.exp| void { $(Text* ptrText)->SetFont($(Font* ptrFont), $(int fsize')) } |]
