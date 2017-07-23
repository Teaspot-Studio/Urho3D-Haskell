{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.DropDownList(
    DropDownList
  , dropDownListContext
  , SharedDropDownList
  , dropDownListAddItem
  , dropDownListInsertItem
  , DropDownListRemoveItem(..)
  , dropDownListRemoveAllItems
  , dropDownListSetSelection
  , dropDownListSetPlaceholderText
  , dropDownListSetResizePopup
  , dropDownListGetNumItems
  , dropDownListGetItem
  , dropDownListGetItems
  , dropDownListGetSelection
  , dropDownListGetSelectedItem
  , dropDownListGetListView
  , dropDownListGetPlaceholder
  , dropDownListGetPlaceholderText
  , dropDownListGetResizePopup
  , dropDownListSetSelectionAttr
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.DropDownList
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.ListView
import Graphics.Urho3D.UI.Menu

C.context (C.cppCtx
  <> sharedDropDownListPtrCntx
  <> dropDownListCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> vector2Context
  <> menuContext
  <> listViewContext
  )
C.include "<Urho3D/UI/DropDownList.h>"
C.using "namespace Urho3D"

C.verbatim "typedef PODVector<UIElement*> PODVectorUIElementPtr;"

dropDownListContext :: C.Context
dropDownListContext = sharedDropDownListPtrCntx
  <> dropDownListCntx

instance Creatable (Ptr DropDownList) where
  type CreationOptions (Ptr DropDownList) = Ptr Context

  newObject ptr = liftIO $ [C.exp| DropDownList* { new DropDownList( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(DropDownList* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage, ''Menu] ''DropDownList

instance UIElem DropDownList where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { DropDownList::GetTypeStatic().Value() } |]

sharedPtr "DropDownList"


-- | Add item to the end of the list.
-- void AddItem(UIElement* item);
dropDownListAddItem :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> Ptr UIElement
  -> m ()
dropDownListAddItem p v = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(DropDownList* ptr)->AddItem($(UIElement* v))} |]

-- | Insert item to a specific position.
-- void InsertItem(unsigned index, UIElement* item);
dropDownListInsertItem :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> Word -- ^ index
  -> Ptr UIElement -- ^ item
  -> m ()
dropDownListInsertItem p i v = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void {$(DropDownList* ptr)->InsertItem($(unsigned int i'), $(UIElement* v))} |]

class DropDownListRemoveItem a where
  -- | Remove specific item.
  dropDownListRemoveItem :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
    => ptr -- ^ Pointer to DropDownList or ascentor
    -> a
    -> m ()

-- | void RemoveItem(UIElement* item);
instance DropDownListRemoveItem (Ptr UIElement) where
  dropDownListRemoveItem p v = liftIO $ do
    let ptr = parentPointer p
    [C.exp| void {$(DropDownList* ptr)->RemoveItem($(UIElement* v))} |]
  {-# INLINE dropDownListRemoveItem #-}

-- | void RemoveItem(unsigned index);
instance DropDownListRemoveItem Word where
  dropDownListRemoveItem p i = liftIO $ do
    let ptr = parentPointer p
        i' = fromIntegral i
    [C.exp| void {$(DropDownList* ptr)->RemoveItem($(unsigned int i'))} |]
  {-# INLINE dropDownListRemoveItem #-}

-- | Remove all items.
-- void RemoveAllItems();
dropDownListRemoveAllItems :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> m ()
dropDownListRemoveAllItems p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(DropDownList* ptr)->RemoveAllItems()} |]

-- | Set selection.
-- void SetSelection(unsigned index);
dropDownListSetSelection :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> Word -- ^ index
  -> m ()
dropDownListSetSelection p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void {$(DropDownList* ptr)->SetSelection($(unsigned int i'))} |]

-- | Set place holder text. This is the text shown when there is no selection (-1) in drop down list. Note that if the list has items, the default is to show the first item, so the "no selection" state has to be set explicitly.
-- void SetPlaceholderText(const String& text);
dropDownListSetPlaceholderText :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> String -- ^ text
  -> m ()
dropDownListSetPlaceholderText p str = liftIO $ withCString str $ \str' -> do
  let ptr = parentPointer p
  [C.exp| void {$(DropDownList* ptr)->SetPlaceholderText(String($(const char* str')))} |]

-- | Set whether popup should be automatically resized to match the dropdown button width.
-- void SetResizePopup(bool enable);
dropDownListSetResizePopup :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> Bool -- ^ enable
  -> m ()
dropDownListSetResizePopup p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(DropDownList* ptr)->SetResizePopup($(int v')!=0)} |]

-- | Return number of items.
-- unsigned GetNumItems() const;
dropDownListGetNumItems :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> m Word
dropDownListGetNumItems p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(DropDownList* ptr)->GetNumItems()} |]

-- | Return item at index.
-- UIElement* GetItem(unsigned index) const;
dropDownListGetItem :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> Word -- ^ index
  -> m (Maybe (Ptr UIElement))
dropDownListGetItem p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  wrapNullPtr <$> [C.exp| UIElement* {$(DropDownList* ptr)->GetItem($(unsigned int i'))} |]

-- | Return all items.
-- PODVector<UIElement*> GetItems() const;
dropDownListGetItems :: (Parent DropDownList a, Pointer ptr a, MonadIO m, ForeignVector v (Ptr UIElement))
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> m (v (Ptr UIElement))
dropDownListGetItems p = liftIO $ do
  let ptr = parentPointer p
  vptr <- [C.exp| PODVectorUIElementPtr* {new PODVectorUIElementPtr($(DropDownList* ptr)->GetItems())} |]
  v <- peekForeignVectorAs vptr
  [C.exp| void {delete $(PODVectorUIElementPtr* vptr)} |]
  pure v

-- | Return selection index, or M_MAX_UNSIGNED if none selected.
-- unsigned GetSelection() const;
dropDownListGetSelection :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> m Word
dropDownListGetSelection p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(DropDownList* ptr)->GetSelection()} |]

-- | Return selected item, or null if none selected.
-- UIElement* GetSelectedItem() const;
dropDownListGetSelectedItem :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> m (Maybe (Ptr UIElement))
dropDownListGetSelectedItem p = liftIO $ do
  let ptr = parentPointer p
  wrapNullPtr <$> [C.exp| UIElement* {$(DropDownList* ptr)->GetSelectedItem()} |]

-- | Return listview element.
-- ListView* GetListView() const { return listView_; }
dropDownListGetListView :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> m (Ptr ListView)
dropDownListGetListView p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| ListView* {$(DropDownList* ptr)->GetListView()} |]

-- | Return selected item placeholder element.
-- UIElement* GetPlaceholder() const { return placeholder_; }
dropDownListGetPlaceholder :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> m (Ptr UIElement)
dropDownListGetPlaceholder p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| UIElement* {$(DropDownList* ptr)->GetPlaceholder()} |]

-- | Return place holder text.
-- const String& GetPlaceholderText() const;
dropDownListGetPlaceholderText :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> m String
dropDownListGetPlaceholderText p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* {$(DropDownList* ptr)->GetPlaceholderText().CString()} |]

-- | Return whether popup should be automatically resized.
-- bool GetResizePopup() const { return resizePopup_; }
dropDownListGetResizePopup :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> m Bool
dropDownListGetResizePopup p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(DropDownList* ptr)->GetResizePopup()} |]

-- | Set selection attribute.
-- void SetSelectionAttr(unsigned index);
dropDownListSetSelectionAttr :: (Parent DropDownList a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to DropDownList or ascentor
  -> Word -- ^ index
  -> m ()
dropDownListSetSelectionAttr p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void {$(DropDownList* ptr)->SetSelectionAttr($(unsigned int i'))} |]
