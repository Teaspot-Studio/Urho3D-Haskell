{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.MessageBox(
    MessageBox
  , MessageBoxCreate(..)
  , defaultMessageBoxCreate
  , HasContext(..)
  , HasMessageString(..)
  , HasMessageTitleString(..)
  , HasMessageLayoutFile(..)
  , HasMessageStyleFile(..)
  , messageBoxContext
  , SharedMessageBox
  , messageBoxSetTitle
  , messageBoxSetMessage
  , messageBoxGetTitle
  , messageBoxGetMessage
  , messageBoxGetWindow
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Control.Lens hiding (Context)
import Data.Monoid
import Foreign
import Foreign.C.String
import GHC.Generics
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Resource.XMLFile
import Graphics.Urho3D.UI.Internal.MessageBox

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx
  <> sharedMessageBoxPtrCntx
  <> contextContext
  <> messageBoxCntx
  <> objectContext
  <> uiElementContext
  <> xmlFileContext
  )
C.include "<Urho3D/UI/MessageBox.h>"
C.using "namespace Urho3D"

messageBoxContext :: C.Context
messageBoxContext = sharedMessageBoxPtrCntx
  <> messageBoxCntx

-- | Constuction options for 'MessageBox'.
-- If layout file is not given, use the default message box layout.
-- If style file is not given, use the default style file from root UI element.
data MessageBoxCreate = MessageBoxCreate {
  _messageBoxCreateContext            :: !(Ptr Context)
, _messageBoxCreateMessageString      :: !String
, _messageBoxCreateMessageTitleString :: !String
, _messageBoxCreateMessageLayoutFile  :: !(Ptr XMLFile)
, _messageBoxCreateMessageStyleFile   :: !(Ptr XMLFile)
} deriving (Show, Generic)
makeFields ''MessageBoxCreate

-- | Make 'MessageBox' creation options with default fields
defaultMessageBoxCreate :: Ptr Context -> MessageBoxCreate
defaultMessageBoxCreate ptr = MessageBoxCreate {
    _messageBoxCreateContext = ptr
  , _messageBoxCreateMessageString = ""
  , _messageBoxCreateMessageTitleString = ""
  , _messageBoxCreateMessageLayoutFile = nullPtr
  , _messageBoxCreateMessageStyleFile = nullPtr
  }

instance Creatable (Ptr MessageBox) where
  type CreationOptions (Ptr MessageBox) = MessageBoxCreate

  newObject MessageBoxCreate{..} = liftIO $
    withCString _messageBoxCreateMessageString $ \_messageBoxCreateMessageString' ->
    withCString _messageBoxCreateMessageTitleString $ \_messageBoxCreateMessageTitleString' ->
    [C.exp| MessageBox* { new MessageBox(
        $(Context* _messageBoxCreateContext)
      , String($(const char* _messageBoxCreateMessageString'))
      , String($(const char* _messageBoxCreateMessageTitleString'))
      , $(XMLFile* _messageBoxCreateMessageLayoutFile)
      , $(XMLFile* _messageBoxCreateMessageStyleFile)
      ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(MessageBox* ptr) } |]

deriveParents [''Object] ''MessageBox

sharedPtr "MessageBox"

-- | Set title text. No-ops if there is no title text element.
-- void SetTitle(const String& text);
messageBoxSetTitle :: (Parent MessageBox a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to MessageBox or ascentor
  -> String
  -> m ()
messageBoxSetTitle p str = liftIO $ withCString str $ \str' -> do
  let ptr = parentPointer p
  [C.exp| void {$(MessageBox* ptr)->SetTitle(String($(const char* str')))} |]

-- | Set message text. No-ops if there is no message text element.
-- void SetMessage(const String& text);
messageBoxSetMessage :: (Parent MessageBox a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to MessageBox or ascentor
  -> String
  -> m ()
messageBoxSetMessage p str = liftIO $ withCString str $ \str' -> do
  let ptr = parentPointer p
  [C.exp| void {$(MessageBox* ptr)->SetMessage(String($(const char* str')))} |]

-- | Return title text. Return empty string if there is no title text element.
-- const String& GetTitle() const;
messageBoxGetTitle :: (Parent MessageBox a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to MessageBox or ascentor
  -> m String
messageBoxGetTitle p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* {$(MessageBox* ptr)->GetTitle().CString()} |]

-- | Return message text. Return empty string if there is no message text element.
-- const String& GetMessage() const;
messageBoxGetMessage :: (Parent MessageBox a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to MessageBox or ascentor
  -> m String
messageBoxGetMessage p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* {$(MessageBox* ptr)->GetMessage().CString()} |]

-- | Return dialog window.
-- UIElement* GetWindow() const { return window_; }
messageBoxGetWindow :: (Parent MessageBox a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to MessageBox or ascentor
  -> m (Ptr UIElement)
messageBoxGetWindow p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| UIElement* {$(MessageBox* ptr)->GetWindow()} |]
