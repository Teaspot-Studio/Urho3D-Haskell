{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.ListView(
    ListView
  , listViewContext
  , SharedListView
  , HighlightMode(..)
  , listViewAddItem
  , listViewInsertItem
  , listViewRemoveItem
  , listViewRemoveItemByIndex
  , listViewRemoveAllItems
  , listViewSetSelection
  , listViewSetSelections
  , listViewAddSelection
  , listViewRemoveSelection
  , listViewToggleSelection
  , listViewChangeSelection
  , listViewClearSelection
  , listViewSetHighlightMode
  , listViewSetMultiselect
  , listViewSetHierarchyMode
  , listViewSetBaseIndent
  , listViewSetClearSelectionOnDefocus
  , listViewSetSelectOnClickEnd
  , listViewExpand
  , listViewToggleExpand
  , listViewGetNumItems
  , listViewGetItem
  , listViewGetItems
  , listViewFindItem
  , listViewGetSelection
  , listViewGetSelections
  , listViewCopySelectedItemsToClipboard
  , listViewGetSelectedItem
  , listViewGetSelectedItems
  , listViewIsSelected
  , listViewIsExpanded
  , listViewGetHighlightMode
  , listViewGetMultiselect
  , listViewGetClearSelectionOnDefocus
  , listViewGetSelectOnClickEnd
  , listViewGetHierarchyMode
  , listViewGetBaseIndent
  , ListViewEnsureItemVisibility(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Maybe
import Data.Monoid
import Foreign
import GHC.Generics
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.UI.Internal.ListView
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Container.Vector.Common
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.ScrollView

C.context (C.cppCtx
  <> sharedListViewPtrCntx
  <> listViewCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> objectContext
  <> animatableContext
  <> serializableContext
  <> vector2Context
  <> scrollViewContext
  <> vectorContext
  )
C.include "<Urho3D/UI/ListView.h>"
C.using "namespace Urho3D"

C.verbatim "typedef PODVector<UIElement*> PODVectorUIElementPtr;"
C.verbatim "typedef PODVector<unsigned> PODVectorWord;"

listViewContext :: C.Context
listViewContext = sharedListViewPtrCntx
  <> listViewCntx

instance Creatable (Ptr ListView) where
  type CreationOptions (Ptr ListView) = Ptr Context

  newObject ptr = liftIO $ [C.exp| ListView* { new ListView( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(ListView* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''ScrollView] ''ListView

instance UIElem ListView where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { ListView::GetTypeStatic().Value() } |]

sharedPtr "ListView"

-- | 'ListView' selection highlight mode.
data HighlightMode =
  -- | Never highlight selections.
    HighlightModeNever
  -- | Highlight when focused.
  | HighlightModeFocus
  -- | Highlight always.
  | HighlightModeAlways
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

-- | Add item to the end of the list.
-- void AddItem(UIElement* item);
listViewAddItem :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Ptr UIElement -- ^ item
  -> m ()
listViewAddItem p v = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(ListView* ptr)->AddItem($(UIElement* v))} |]

-- | Insert item at a specific index. In hierarchy mode, the optional parameter will be used to determine the child's indent level in respect to its parent.
-- If index is greater than the total items then the new item is inserted at the end of the list.
-- In hierarchy mode, if index is greater than the index of last children of the specified parent item then the new item is inserted next to the last children.
-- And if the index is lesser than the index of the parent item itself then the new item is inserted before the first child item.
-- void InsertItem(unsigned index, UIElement* item, UIElement* parentItem = 0);
listViewInsertItem :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> Ptr UIElement -- ^ item
  -> Maybe (Ptr UIElement) -- ^ parent item
  -> m ()
listViewInsertItem p i item mp = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
      parent = fromMaybe nullPtr mp
  [C.exp| void {$(ListView* ptr)->InsertItem($(unsigned int i'), $(UIElement* item), $(UIElement* parent))} |]

-- | Remove specific item, starting search at the specified index if provided. In hierarchy mode will also remove any children.
-- void RemoveItem(UIElement* item, unsigned index = 0);
listViewRemoveItem :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Ptr UIElement -- ^ item
  -> Word -- ^ index (default 0)
  -> m ()
listViewRemoveItem p item index = liftIO $ do
  let ptr = parentPointer p
      index' = fromIntegral index
  [C.exp| void {$(ListView* ptr)->RemoveItem($(UIElement* item), $(unsigned int index'))} |]

-- | Remove item at index. In hierarchy mode will also remove any children.
-- void RemoveItem(unsigned index);
listViewRemoveItemByIndex :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> m ()
listViewRemoveItemByIndex p index = liftIO $ do
  let ptr = parentPointer p
      index' = fromIntegral index
  [C.exp| void {$(ListView* ptr)->RemoveItem($(unsigned int index'))} |]

-- | Remove all items.
-- void RemoveAllItems();
listViewRemoveAllItems :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m ()
listViewRemoveAllItems p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(ListView* ptr)->RemoveAllItems()} |]

-- | Set selection.
-- void SetSelection(unsigned index);
listViewSetSelection :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> m ()
listViewSetSelection p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void {$(ListView* ptr)->SetSelection($(unsigned int v'))} |]

-- | Set multiple selected items. If multiselect disabled, sets only the first.
-- void SetSelections(const PODVector<unsigned>& indices);
listViewSetSelections :: (Parent ListView a, Pointer ptr a, MonadIO m, Foldable v, ForeignElemConstr v Word, ForeignVectorRepresent v)
  => ptr -- ^ Pointer to listView or ascentor
  -> v Word
  -> m ()
listViewSetSelections p v = liftIO $ withForeignVector () v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(ListView* ptr)->SetSelections(*$(PODVectorWord* v'))} |]

-- | Add item to the selection, multiselect mode only.
-- void AddSelection(unsigned index);
listViewAddSelection :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> m ()
listViewAddSelection p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void {$(ListView* ptr)->AddSelection($(unsigned int i'))} |]

-- | Remove item from the selection.
-- void RemoveSelection(unsigned index);
listViewRemoveSelection :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> m ()
listViewRemoveSelection p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void {$(ListView* ptr)->RemoveSelection($(unsigned int i'))} |]

-- | Toggle selection of an item.
-- void ToggleSelection(unsigned index);
listViewToggleSelection :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> m ()
listViewToggleSelection p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void {$(ListView* ptr)->ToggleSelection($(unsigned int i'))} |]

-- | Move selection by a delta and clamp at list ends. If additive (multiselect only), will add to the existing selection.
-- void ChangeSelection(int delta, bool additive = false);
listViewChangeSelection :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Int -- ^ delta
  -> Bool -- ^ additive (default False)
  -> m ()
listViewChangeSelection p d a = liftIO $ do
  let ptr = parentPointer p
      d' = fromIntegral d
      a' = fromBool a
  [C.exp| void {$(ListView* ptr)->ChangeSelection($(int d'), $(int a') != 0)} |]

-- | Clear selection.
-- void ClearSelection();
listViewClearSelection :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m ()
listViewClearSelection p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(ListView* ptr)->ClearSelection()} |]

-- | Set selected items' highlight mode.
-- void SetHighlightMode(HighlightMode mode);
listViewSetHighlightMode :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> HighlightMode -- ^ mode
  -> m ()
listViewSetHighlightMode p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral . fromEnum $ v
  [C.exp| void {$(ListView* ptr)->SetHighlightMode((HighlightMode)$(int v'))} |]

-- | Enable multiselect.
-- void SetMultiselect(bool enable);
listViewSetMultiselect :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Bool -- ^ enable
  -> m ()
listViewSetMultiselect p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(ListView* ptr)->SetMultiselect($(int v') != 0)} |]

-- | Enable hierarchy mode. Allows items to have parent-child relationship at different indent level and the ability to expand/collapse child items.
-- All items in the list will be lost during mode change.
-- void SetHierarchyMode(bool enable);
listViewSetHierarchyMode :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Bool -- ^ enable
  -> m ()
listViewSetHierarchyMode p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(ListView* ptr)->SetHierarchyMode($(int v') != 0)} |]

-- | Set base indent, i.e. the indent level of the ultimate parent item.
-- void SetBaseIndent(int baseIndent);
listViewSetBaseIndent :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Int -- ^ base indent
  -> m ()
listViewSetBaseIndent p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  [C.exp| void {$(ListView* ptr)->SetBaseIndent($(int v'))} |]

-- | Enable clearing of selection on defocus.
-- void SetClearSelectionOnDefocus(bool enable);
listViewSetClearSelectionOnDefocus :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Bool -- ^ enable
  -> m ()
listViewSetClearSelectionOnDefocus p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(ListView* ptr)->SetClearSelectionOnDefocus($(int v') != 0)} |]

-- | Enable reacting to click end instead of click start for item selection. Default false.
-- void SetSelectOnClickEnd(bool enable);
listViewSetSelectOnClickEnd :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Bool -- ^ enable
  -> m ()
listViewSetSelectOnClickEnd p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(ListView* ptr)->SetSelectOnClickEnd($(int v') != 0)} |]

-- | Expand item at index. Only has effect in hierarchy mode.
-- void Expand(unsigned index, bool enable, bool recursive = false);
listViewExpand :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> Bool -- ^ enable
  -> Bool -- ^ recursive (default False)
  -> m ()
listViewExpand p i e r = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
      e' = fromBool e
      r' = fromBool r
  [C.exp| void {$(ListView* ptr)->Expand($(unsigned int i'), $(int e') != 0, $(int r') != 0)} |]

-- | Toggle item's expanded flag at index. Only has effect in hierarchy mode.
-- void ToggleExpand(unsigned index, bool recursive = false);
listViewToggleExpand :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> Bool -- ^ recursive (default false)
  -> m ()
listViewToggleExpand p i r = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
      r' = fromBool r
  [C.exp| void {$(ListView* ptr)->ToggleExpand($(unsigned int i'), $(int r') != 0)} |]

-- | Return number of items.
-- unsigned GetNumItems() const;
listViewGetNumItems :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m Word
listViewGetNumItems p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(ListView* ptr)->GetNumItems()} |]

-- | Return item at index.
-- UIElement* GetItem(unsigned index) const;
listViewGetItem :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> m (Maybe (Ptr UIElement))
listViewGetItem p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  wrapNullPtr <$> [C.exp| UIElement* {$(ListView* ptr)->GetItem($(unsigned int v'))} |]

-- | Return all items.
-- PODVector<UIElement*> GetItems() const;
listViewGetItems :: (Parent ListView a, Pointer ptr a, MonadIO m, ForeignVector v (Ptr UIElement))
  => ptr -- ^ Pointer to listView or ascentor
  -> m (v (Ptr UIElement))
listViewGetItems p = liftIO $ do
  let ptr = parentPointer p
  vptr <- [C.exp| PODVectorUIElementPtr* {new PODVectorUIElementPtr($(ListView* ptr)->GetItems())} |]
  v <- peekForeignVectorAs vptr
  [C.exp| void { delete $(PODVectorUIElementPtr* vptr) }|]
  pure v

-- | Return index of item, or M_MAX_UNSIGNED If not found.
-- unsigned FindItem(UIElement* item) const;
listViewFindItem :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Ptr UIElement -- ^ item
  -> m Word
listViewFindItem p v = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(ListView* ptr)->FindItem($(UIElement* v))} |]

-- | Return first selected index, or M_MAX_UNSIGNED if none selected.
-- unsigned GetSelection() const;
listViewGetSelection :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m Word
listViewGetSelection p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int {$(ListView* ptr)->GetSelection()} |]

-- | Return all selected indices.
-- const PODVector<unsigned>& GetSelections() const { return selections_; }
listViewGetSelections :: (Parent ListView a, Pointer ptr a, MonadIO m, ForeignVector v Word)
  => ptr -- ^ Pointer to listView or ascentor
  -> m (v Word)
listViewGetSelections p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(ListView* ptr)->GetSelections()} |]
  vptr <- [C.exp| PODVectorWord* {new PODVectorWord($(ListView* ptr)->GetSelections())} |]
  v <- peekForeignVectorAs vptr
  [C.exp| void { delete $(PODVectorWord* vptr) }|]
  pure v

-- | Copy selected items to system clipboard. Currently only applicable to Text items.
-- void CopySelectedItemsToClipboard() const;
listViewCopySelectedItemsToClipboard :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m ()
listViewCopySelectedItemsToClipboard p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(ListView* ptr)->CopySelectedItemsToClipboard()} |]

-- | Return first selected item, or null if none selected.
-- UIElement* GetSelectedItem() const;
listViewGetSelectedItem :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m (Maybe (Ptr UIElement))
listViewGetSelectedItem p = liftIO $ do
  let ptr = parentPointer p
  wrapNullPtr <$> [C.exp| UIElement* {$(ListView* ptr)->GetSelectedItem()} |]

-- | Return all selected items.
-- PODVector<UIElement*> GetSelectedItems() const;
listViewGetSelectedItems :: (Parent ListView a, Pointer ptr a, MonadIO m, ForeignVector v (Ptr UIElement))
  => ptr -- ^ Pointer to listView or ascentor
  -> m (v (Ptr UIElement))
listViewGetSelectedItems p = liftIO $ do
  let ptr = parentPointer p
  vptr <- [C.exp| PODVectorUIElementPtr* {new PODVectorUIElementPtr($(ListView* ptr)->GetSelectedItems())} |]
  v <- peekForeignVectorAs vptr
  [C.exp| void { delete $(PODVectorUIElementPtr* vptr) }|]
  pure v

-- | Return whether an item at index is seleccted.
-- bool IsSelected(unsigned index) const;
listViewIsSelected :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> m Bool
listViewIsSelected p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  toBool <$> [C.exp| int {(int)$(ListView* ptr)->IsSelected($(unsigned int v'))} |]

-- | Return whether an item at index has its children expanded (in hierarchy mode only).
-- bool IsExpanded(unsigned index) const;
listViewIsExpanded :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> Word -- ^ index
  -> m Bool
listViewIsExpanded p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromIntegral v
  toBool <$> [C.exp| int {(int)$(ListView* ptr)->IsExpanded($(unsigned int v'))} |]

-- | Return highlight mode.
-- HighlightMode GetHighlightMode() const { return highlightMode_; }
listViewGetHighlightMode :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m HighlightMode
listViewGetHighlightMode p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int {(int)$(ListView* ptr)->GetHighlightMode()} |]

-- | Return whether multiselect enabled.
-- bool GetMultiselect() const { return multiselect_; }
listViewGetMultiselect :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m Bool
listViewGetMultiselect p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(ListView* ptr)->GetMultiselect()} |]

-- | Return whether selection is cleared on defocus.
-- bool GetClearSelectionOnDefocus() const { return clearSelectionOnDefocus_; }
listViewGetClearSelectionOnDefocus :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m Bool
listViewGetClearSelectionOnDefocus p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(ListView* ptr)->GetClearSelectionOnDefocus()} |]

-- | Return whether reacts to click end instead of click start for item selection.
-- bool GetSelectOnClickEnd() const { return selectOnClickEnd_; }
listViewGetSelectOnClickEnd :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m Bool
listViewGetSelectOnClickEnd p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(ListView* ptr)->GetSelectOnClickEnd()} |]

-- | Return whether hierarchy mode enabled.
-- bool GetHierarchyMode() const { return hierarchyMode_; }
listViewGetHierarchyMode :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m Bool
listViewGetHierarchyMode p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(ListView* ptr)->GetHierarchyMode()} |]

-- | Return base indent.
-- int GetBaseIndent() const { return baseIndent_; }
listViewGetBaseIndent :: (Parent ListView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to listView or ascentor
  -> m Int
listViewGetBaseIndent p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int {$(ListView* ptr)->GetBaseIndent()} |]

class ListViewEnsureItemVisibility a where
  -- | Ensure full visibility of the item.
  listViewEnsureItemVisibility :: (Parent ListView a, Pointer ptr a, MonadIO m)
    => ptr -- ^ Pointer to listView or ascentor
    -> a -- ^ value by which the implementation checks visiblity
    -> m ()

-- | void EnsureItemVisibility(unsigned index);
instance ListViewEnsureItemVisibility Word where
  listViewEnsureItemVisibility p v = liftIO $ do
    let ptr = parentPointer p
        v' = fromIntegral v
    [C.exp| void {$(ListView* ptr)->EnsureItemVisibility($(unsigned int v'))} |]
  {-# INLINE listViewEnsureItemVisibility #-}

-- | void EnsureItemVisibility(UIElement* item);
instance ListViewEnsureItemVisibility (Ptr UIElement) where
  listViewEnsureItemVisibility p v = liftIO $ do
    let ptr = parentPointer p
    [C.exp| void {$(ListView* ptr)->EnsureItemVisibility($(UIElement* v))} |]
  {-# INLINE listViewEnsureItemVisibility #-}
