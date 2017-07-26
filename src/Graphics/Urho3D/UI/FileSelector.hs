{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.FileSelector(
    FileSelector
  , fileSelectorContext
  , SharedFileSelector
  , WeakFileSelector
  , FileSelectorEntry(..)
  , HasName(..)
  , HasDirectory(..)
  , fileSelectorSetDefaultStyle
  , fileSelectorSetTitle
  , fileSelectorSetButtonTexts
  , fileSelectorSetPath
  , fileSelectorSetFileName
  , fileSelectorSetFilters
  , fileSelectorSetDirectoryMode
  , fileSelectorUpdateElements
  , fileSelectorGetDefaultStyle
  , fileSelectorGetWindow
  , fileSelectorGetTitleText
  , fileSelectorGetFileList
  , fileSelectorGetPathEdit
  , fileSelectorGetFileNameEdit
  , fileSelectorGetFilterList
  , fileSelectorGetOKButton
  , fileSelectorGetCancelButton
  , fileSelectorGetCloseButton
  , fileSelectorGetTitle
  , fileSelectorGetPath
  , fileSelectorGetFileName
  , fileSelectorGetFilter
  , fileSelectorGetFilterIndex
  , fileSelectorGetDirectoryMode
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Foreign.C.String
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.FileSelector
import Text.RawString.QQ

import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.XMLFile
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Button
import Graphics.Urho3D.UI.DropDownList
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.LineEdit
import Graphics.Urho3D.UI.ListView
import Graphics.Urho3D.UI.Text
import Graphics.Urho3D.UI.Window

C.context (C.cppCtx
  <> sharedFileSelectorPtrCntx
  <> weakFileSelectorPtrCntx
  <> fileSelectorCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> objectContext
  <> animatableContext
  <> serializableContext
  <> xmlFileContext
  <> vectorContext
  <> windowContext
  <> lineEditContext
  <> listViewContext
  <> textContext
  <> dropDownListContext
  <> buttonContext
  )
C.include "<Urho3D/UI/FileSelector.h>"
C.using "namespace Urho3D"

fileSelectorContext :: C.Context
fileSelectorContext = sharedFileSelectorPtrCntx
  <> weakFileSelectorPtrCntx
  <> fileSelectorCntx

instance Creatable (Ptr FileSelector) where
  type CreationOptions (Ptr FileSelector) = Ptr Context

  newObject ptr = liftIO $ [C.exp| FileSelector* { new FileSelector( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(FileSelector* ptr) } |]

deriveParents [''Object] ''FileSelector

sharedPtr "FileSelector"
sharedWeakPtr "FileSelector"

C.verbatim "typedef Vector<String> VectorString;"

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

instance Storable FileSelectorEntry where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(FileSelectorEntry) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<FileSelectorEntry>::AlignmentOf } |]
  peek ptr = do
    _fileSelectorEntryName <- peekCString =<< [C.exp| const char* { $(FileSelectorEntry* ptr)->name_.CString() } |]
    _fileSelectorEntryDirectory <- toBool <$> [C.exp| int { (int)$(FileSelectorEntry* ptr)->directory_ } |]
    return FileSelectorEntry {..}
  poke ptr FileSelectorEntry {..} = withCString _fileSelectorEntryName $ \_fileSelectorEntryName' ->
    [C.block| void {
      $(FileSelectorEntry* ptr)->name_ = String($(const char* _fileSelectorEntryName'));
      $(FileSelectorEntry* ptr)->directory_ = $(int _fileSelectorEntryDirectory') != 0;
    } |]
    where
    _fileSelectorEntryDirectory' = fromBool _fileSelectorEntryDirectory

-- | Set fileselector UI style.
-- void SetDefaultStyle(XMLFile* style);
fileSelectorSetDefaultStyle :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> Ptr XMLFile -- ^ style
  -> m ()
fileSelectorSetDefaultStyle p v = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(FileSelector* ptr)->SetDefaultStyle($(XMLFile* v))} |]

-- | Set title text.
-- void SetTitle(const String& text);
fileSelectorSetTitle :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> String
  -> m ()
fileSelectorSetTitle p v = liftIO $ withCString v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(FileSelector* ptr)->SetTitle(String($(const char* v')))} |]

-- | Set button texts.
-- void SetButtonTexts(const String& okText, const String& cancelText);
fileSelectorSetButtonTexts :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> String -- ^ ok text
  -> String -- ^ cancel text
  -> m ()
fileSelectorSetButtonTexts p v1 v2 = liftIO $ withCString v1 $ \v1' -> withCString v2 $ \v2' -> do
  let ptr = parentPointer p
  [C.exp| void {$(FileSelector* ptr)->SetButtonTexts(String($(const char* v1')), String($(const char* v2')))} |]

-- | Set current path.
-- void SetPath(const String& path);
fileSelectorSetPath :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> String -- ^ path
  -> m ()
fileSelectorSetPath p v = liftIO $ withCString v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(FileSelector* ptr)->SetPath(String($(const char* v')))} |]

-- | Set current filename.
-- void SetFileName(const String& fileName);
fileSelectorSetFileName :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> String -- ^ file name
  -> m ()
fileSelectorSetFileName p v = liftIO $ withCString v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(FileSelector* ptr)->SetFileName(String($(const char* v')))} |]

-- | Set filters.
-- void SetFilters(const Vector<String>& filters, unsigned defaultIndex);
fileSelectorSetFilters :: (Parent FileSelector a, Pointer ptr a, MonadIO m, ForeignVector v String)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> v String -- ^ filters
  -> Word -- ^ default index
  -> m ()
fileSelectorSetFilters p vs i = liftIO $ withForeignVector () vs $ \vs' -> do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void {$(FileSelector* ptr)->SetFilters(*$(VectorString* vs'), $(unsigned int i'))} |]

-- | Set directory selection mode. Default false.
-- void SetDirectoryMode(bool enable);
fileSelectorSetDirectoryMode :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> Bool -- ^ enable
  -> m ()
fileSelectorSetDirectoryMode p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(FileSelector* ptr)->SetDirectoryMode($(int v') != 0)} |]

-- | Update elements to layout properly. Call this after manually adjusting the sub-elements.
-- void UpdateElements();
fileSelectorUpdateElements :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m ()
fileSelectorUpdateElements p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(FileSelector* ptr)->UpdateElements()} |]

-- | Return the UI style file.
-- XMLFile* GetDefaultStyle() const;
fileSelectorGetDefaultStyle :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr XMLFile)
fileSelectorGetDefaultStyle p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| XMLFile* {$(FileSelector* ptr)->GetDefaultStyle()} |]

-- | Return fileselector window.
-- Window* GetWindow() const { return window_; }
fileSelectorGetWindow :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr Window)
fileSelectorGetWindow p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Window* {$(FileSelector* ptr)->GetWindow()} |]

-- | Return window title text element.
-- Text* GetTitleText() const { return titleText_; }
fileSelectorGetTitleText :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr Text)
fileSelectorGetTitleText p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Text* {$(FileSelector* ptr)->GetTitleText()} |]

-- | Return file list.
-- ListView* GetFileList() const { return fileList_; }
fileSelectorGetFileList :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr ListView)
fileSelectorGetFileList p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| ListView* {$(FileSelector* ptr)->GetFileList()} |]

-- | Return path editor.
-- LineEdit* GetPathEdit() const { return pathEdit_; }
fileSelectorGetPathEdit :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr LineEdit)
fileSelectorGetPathEdit p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| LineEdit* {$(FileSelector* ptr)->GetPathEdit()} |]

-- | Return filename editor.
-- LineEdit* GetFileNameEdit() const { return fileNameEdit_; }
fileSelectorGetFileNameEdit :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr LineEdit)
fileSelectorGetFileNameEdit p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| LineEdit* {$(FileSelector* ptr)->GetFileNameEdit()} |]

-- | Return filter dropdown.
-- DropDownList* GetFilterList() const { return filterList_; }
fileSelectorGetFilterList :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr DropDownList)
fileSelectorGetFilterList p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| DropDownList* {$(FileSelector* ptr)->GetFilterList()} |]

-- | Return OK button.
-- Button* GetOKButton() const { return okButton_; }
fileSelectorGetOKButton :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr Button)
fileSelectorGetOKButton p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Button* {$(FileSelector* ptr)->GetOKButton()} |]

-- | Return cancel button.
-- Button* GetCancelButton() const { return cancelButton_; }
fileSelectorGetCancelButton :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr Button)
fileSelectorGetCancelButton p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Button* {$(FileSelector* ptr)->GetCancelButton()} |]

-- | Return close button.
-- Button* GetCloseButton() const { return closeButton_; }
fileSelectorGetCloseButton :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m (Ptr Button)
fileSelectorGetCloseButton p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| Button* {$(FileSelector* ptr)->GetCloseButton()} |]

-- | Return window title.
-- const String& GetTitle() const;
fileSelectorGetTitle :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m String
fileSelectorGetTitle p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* {$(FileSelector* ptr)->GetTitle().CString()} |]

-- | Return current path.
-- const String& GetPath() const { return path_; }
fileSelectorGetPath :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m String
fileSelectorGetPath p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* {$(FileSelector* ptr)->GetPath().CString()} |]

-- | Return current filename.
-- const String& GetFileName() const;
fileSelectorGetFileName :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m String
fileSelectorGetFileName p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* {$(FileSelector* ptr)->GetFileName().CString()} |]

-- | Return current filter.
-- const String& GetFilter() const;
fileSelectorGetFilter :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m String
fileSelectorGetFilter p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* {$(FileSelector* ptr)->GetFilter().CString()} |]

-- | Return current filter index.
-- unsigned GetFilterIndex() const;
fileSelectorGetFilterIndex :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m Word
fileSelectorGetFilterIndex p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(FileSelector* ptr)->GetFilterIndex()} |]

-- | Return directory mode flag.
-- bool GetDirectoryMode() const { return directoryMode_; }
fileSelectorGetDirectoryMode :: (Parent FileSelector a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to fileSelector or ascentor
  -> m Bool
fileSelectorGetDirectoryMode p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(FileSelector* ptr)->GetDirectoryMode()} |]
