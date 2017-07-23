{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Element(
    UIElement
  , uiElementContext
  , SharedUIElement
  , WeakUIElement
  , PODVectorUIElementPtr
  , VectorUIElementPtr
  , UIElem(..)
  , Orientation(..)
  , HorizontalAlignment(..)
  , VerticalAlignment(..)
  , Corner(..)
  , FocusMode(..)
  , LayoutMode(..)
  , TraversalMode(..)
  , ddDisabled
  , ddSource
  , ddTarget
  , ddSourceAndTarget

  -- | Setters
  , uiElementSetName
  , uiElementSetPosition
  , uiElementSetSize
  , uiElementSetSize'
  , uiElementSetWidth
  , uiElementSetHeight
  , uiElementSetMinSize
  , uiElementSetMinSize'
  , uiElementSetMinWidth
  , uiElementSetMinHeight
  , uiElementSetMaxSize
  , uiElementSetMaxSize'
  , uiElementSetMaxWidth
  , uiElementSetMaxHeight
  , uiElementSetFixedSize
  , uiElementSetFixedSize'
  , uiElementSetFixedWidth
  , uiElementSetFixedHeight
  , uiElementSetAlignment
  , uiElementSetHorizontalAlignment
  , uiElementSetVerticalAlignment
  , uiElementSetClipBorder
  , uiElementSetColor
  , uiElementSetCornerColor
  , uiElementSetPriority
  , uiElementSetOpacity
  , uiElementSetBringToFront
  , uiElementSetBringToBack
  , uiElementSetClipChildren
  , uiElementSetSortChildren
  , uiElementSetUseDerivedOpacity
  , uiElementSetEnabled
  , uiElementSetDeepEnabled
  , uiElementResetDeepEnabled
  , uiElementSetEnabledRecursive
  , uiElementSetEditable
  , uiElementSetFocus
  , uiElementSetSelected
  , uiElementSetVisible
  , uiElementSetFocusMode
  , uiElementSetDragDropMode
  , uiElementSetStyle
  , uiElementSetStyle'
  , uiElementSetStyleDefault
  , uiElementSetStyleAuto
  , uiElementSetStyleAutoDefault
  , uiElementSetDefaultStyle
  , uiElementSetLayout
  , uiElementSetLayoutMode
  , uiElementSetLayoutSpacing
  , uiElementSetLayoutBorder
  , uiElementSetLayoutFlexScale
  , uiElementSetIndent
  , uiElementSetIndentSpacing
  , uiElementUpdateLayout
  , uiElementDisableLayoutUpdate
  , uiElementEnableLayoutUpdate
  , uiElementBringToFront
  , uiElementCreateChild
  , createChild
  , createChildSimple
  , uiElementAddChild
  , uiElementInsertChild
  , uiElementRemoveChild
  , uiElementRemoveChildAtIndex
  , uiElementRemoveAllChildren
  , uiElementRemove
  , uiElementFindChild
  , uiElementSetParent
  , uiElementSetVar
  , uiElementSetInternal
  , uiElementSetTraversalMode
  , uiElementSetElementEventSender
  , uiElementSetChildOffset
  , uiElementSetHovering
  , uiElementAdjustScissor

  -- | Getters
  , uiElementGetName
  , uiElementGetPosition
  , uiElementGetSize
  , uiElementGetWidth
  , uiElementGetHeight
  , uiElementGetMinSize
  , uiElementGetMinWidth
  , uiElementGetMinHeight
  , uiElementGetMaxSize
  , uiElementGetMaxWidth
  , uiElementGetMaxHeight
  , uiElementIsFixedSize
  , uiElementIsFixedWidth
  , uiElementIsFixedHeight
  , uiElementGetChildOffset
  , uiElementGetHorizontalAlignment
  , uiElementGetVerticalAlignment
  , uiElementGetClipBorder
  , uiElementGetColor
  , uiElementGetPriority
  , uiElementGetOpacity
  , uiElementGetDerivedOpacity
  , uiElementGetBringToFront
  , uiElementGetBringToBack
  , uiElementGetClipChildren
  , uiElementGetSortChildren
  , uiElementGetUseDerivedOpacity
  , uiElementHasFocus
  , uiElementIsEnabled
  , uiElementIsEnabledSelf
  , uiElementIsEditable
  , uiElementIsSelected
  , uiElementIsVisible
  , uiElementIsVisibleEffective
  , uiElementIsHovering
  , uiElementIsInternal
  , uiElementHasColorGradient
  , uiElementGetFocusMode
  , uiElementGetDragDropMode
  , uiElementGetAppliedStyle
  , uiElementGetDefaultStyle
  , uiElementGetLayoutMode
  , uiElementGetLayoutSpacing
  , uiElementGetLayoutBorder
  , uiElementGetLayoutFlexScale
  , uiElementGetNumChildren
  , uiElementGetChildByIndex
  , uiElementGetChildByName
  , uiElementGetChildByVariable
  , uiElementGetChildren
  , uiElementGetParent
  , uiElementGetRoot
  , uiElementGetDerivedColor
  , uiElementGetVar
  , uiElementGetVars
  , uiElementGetDragButtonCombo
  , uiElementGetDragButtonCount
  , uiElementScreenToElement
  , uiElementElementToScreen
  , uiElementIsInside
  , uiElementIsInsideCombined
  , uiElementGetCombinedScreenRect
  , uiElementSortChildren
  , uiElementGetIndent
  , uiElementGetIndentSpacing
  , uiElementGetIndentWidth
  , uiElementGetTraversalMode
  , uiElementIsElementEventSender
  , uiElementGetElementEventSender
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Data.Proxy
import Foreign
import Foreign.C.String
import GHC.Generics
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Resource.XMLElement
import Graphics.Urho3D.Resource.XMLFile
import Graphics.Urho3D.UI.Internal.Element

C.context (C.cppCtx
  <> sharedUIElementPtrCntx
  <> weakUIElementPtrCntx
  <> uiElementCntx
  <> stringHashContext
  <> vector2Context
  <> rectContext
  <> contextContext
  <> xmlFileContext
  <> xmlElementContext
  <> stringContext
  <> colorContext
  <> variantContext
  <> podVectorUIElementPtrCntx
  <> vectorUIElementPtrCntx
  )
C.include "<Urho3D/UI/UIElement.h>"
C.using "namespace Urho3D"

uiElementContext :: C.Context
uiElementContext = sharedUIElementPtrCntx <> weakUIElementPtrCntx <> uiElementCntx <> stringHashContext <> podVectorUIElementPtrCntx <> vectorUIElementPtrCntx

C.verbatim "typedef HashMap<StringHash, Variant> HashMapStringHashVariant;"

instance Creatable (Ptr UIElement) where
  type CreationOptions (Ptr UIElement) = Ptr Context

  newObject ptr = liftIO $ [C.exp| UIElement* { new UIElement($(Context* ptr)) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(UIElement* ptr) } |]

sharedPtr "UIElement"
sharedWeakPtr "UIElement"
vectorPtr "UIElement"

-- | UI element orientation.
data Orientation = OrientationHorizontal | OrientationVertical
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

-- | Drag and drop disabled.
ddDisabled :: Word
ddDisabled = fromIntegral [C.pure| unsigned int {DD_DISABLED} |]

-- | Drag and drop source flag.
ddSource :: Word
ddSource = fromIntegral [C.pure| unsigned int {DD_SOURCE} |]

-- | Drag and drop target flag.
ddTarget :: Word
ddTarget = fromIntegral [C.pure| unsigned int {DD_TARGET} |]

-- | Drag and drop source and target.
ddSourceAndTarget :: Word
ddSourceAndTarget = fromIntegral [C.pure| unsigned int {DD_SOURCE_AND_TARGET} |]

-- | Create and add a child element and return it.
uiElementCreateChild :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> StringHash -- ^ Element type
  -> String -- ^ Name
  -> Int -- ^ Index
  -> m (Ptr UIElement)
uiElementCreateChild ptr hash name index = liftIO $ withCString name $ \name' -> with hash $ \hash' -> do
  let ptr' = parentPointer ptr
      index' = fromIntegral index
  [C.exp| UIElement* { $(UIElement* ptr')->CreateChild(*$(StringHash* hash'), String($(const char* name')), $(int index')) } |]

-- | Elements that can be attached to another gui elements
class UIElem a where
  -- | Getting hash id of the element kind
  uiElemType :: Proxy a -> StringHash

createChild :: forall a p m e . (Parent UIElement a, Pointer p a, MonadIO m, UIElem e)
  => p -- ^ Pointer to UI element
  -> String -- ^ Name
  -> Int -- ^ Index
  -> m (Ptr e)
createChild ptr name index = do
  eptr <- uiElementCreateChild ptr (uiElemType (Proxy :: Proxy e)) name index
  return $ castPtr eptr

createChildSimple :: (Parent UIElement a, Pointer p a, MonadIO m, UIElem e) => p -- ^ Pointer to UI element
  -> m (Ptr e)
createChildSimple ptr = createChild ptr "" (fromIntegral [C.pure| unsigned int { M_MAX_UNSIGNED } |])

-- | Changes physical size of element
uiElementSetSize :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ Size vector
  -> m ()
uiElementSetSize ptr v = liftIO $ with v $ \v' -> do
  let ptr' = parentPointer ptr
  [C.exp| void { $(UIElement* ptr')->SetSize(*$(IntVector2* v')) } |]

-- | Changes physical size of element
uiElementSetSize' :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Width
  -> Int -- ^ Height
  -> m ()
uiElementSetSize' ptr width height = liftIO $ do
  let ptr' = parentPointer ptr
      width' = fromIntegral width
      height' = fromIntegral height
  [C.exp| void { $(UIElement* ptr')->SetSize($(int width'), $(int height')) } |]

-- | Set width only.
uiElementSetWidth :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Width value
  -> m ()
uiElementSetWidth p width = liftIO $ do
  let ptr = parentPointer p
      width' = fromIntegral width
  [C.exp| void { $(UIElement* ptr)->SetWidth($(int width')) } |]

-- | Set height only.
uiElementSetHeight :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Height value
  -> m ()
uiElementSetHeight p height = liftIO $ do
  let ptr = parentPointer p
      height' = fromIntegral height
  [C.exp| void { $(UIElement* ptr)->SetWidth($(int height')) } |]

-- | Set minimum width.
uiElementSetMinWidth :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Width value
  -> m ()
uiElementSetMinWidth p w = liftIO $ do
  let ptr = parentPointer p
      w' = fromIntegral w
  [C.exp| void { $(UIElement* ptr)->SetMinWidth($(int w')) } |]

-- | Set minimum height.
uiElementSetMinHeight :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Height value
  -> m ()
uiElementSetMinHeight p h = liftIO $ do
  let ptr = parentPointer p
      h' = fromIntegral h
  [C.exp| void { $(UIElement* ptr)->SetMinHeight($(int h')) } |]

-- | Set minimum height.
uiElementSetMinSize :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ Width and Height
  -> m ()
uiElementSetMinSize p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetMinSize(*$(IntVector2* v')) } |]

-- | Set minimum height.
uiElementSetMinSize' :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Min width
  -> Int -- ^ Min height
  -> m ()
uiElementSetMinSize' p mw mh = liftIO $ do
  let ptr = parentPointer p
      mw' = fromIntegral mw
      mh' = fromIntegral mh
  [C.exp| void { $(UIElement* ptr)->SetMinSize($(int mw'), $(int mh')) } |]

-- | Set maximum width.
uiElementSetMaxWidth :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Width value
  -> m ()
uiElementSetMaxWidth p w = liftIO $ do
  let ptr = parentPointer p
      w' = fromIntegral w
  [C.exp| void { $(UIElement* ptr)->SetMaxWidth($(int w')) } |]

-- | Set maximum height.
uiElementSetMaxHeight :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Height value
  -> m ()
uiElementSetMaxHeight p h = liftIO $ do
  let ptr = parentPointer p
      h' = fromIntegral h
  [C.exp| void { $(UIElement* ptr)->SetMaxHeight($(int h')) } |]

-- | Set maximum height.
uiElementSetMaxSize :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ Width and Height
  -> m ()
uiElementSetMaxSize p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetMaxSize(*$(IntVector2* v')) } |]

-- | Set maximum height.
uiElementSetMaxSize' :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Max width
  -> Int -- ^ Max height
  -> m ()
uiElementSetMaxSize' p mw mh = liftIO $ do
  let ptr = parentPointer p
      mw' = fromIntegral mw
      mh' = fromIntegral mh
  [C.exp| void { $(UIElement* ptr)->SetMaxSize($(int mw'), $(int mh')) } |]

-- | Set fixed width.
uiElementSetFixedWidth :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Width value
  -> m ()
uiElementSetFixedWidth p w = liftIO $ do
  let ptr = parentPointer p
      w' = fromIntegral w
  [C.exp| void { $(UIElement* ptr)->SetFixedWidth($(int w')) } |]

-- | Set fixed height.
uiElementSetFixedHeight :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Height value
  -> m ()
uiElementSetFixedHeight p h = liftIO $ do
  let ptr = parentPointer p
      h' = fromIntegral h
  [C.exp| void { $(UIElement* ptr)->SetFixedHeight($(int h')) } |]

-- | Set fixed height.
uiElementSetFixedSize :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ Width and Height
  -> m ()
uiElementSetFixedSize p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetFixedSize(*$(IntVector2* v')) } |]

-- | Set fixed height.
uiElementSetFixedSize' :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Fixed width
  -> Int -- ^ Fixed height
  -> m ()
uiElementSetFixedSize' p mw mh = liftIO $ do
  let ptr = parentPointer p
      mw' = fromIntegral mw
      mh' = fromIntegral mh
  [C.exp| void { $(UIElement* ptr)->SetFixedSize($(int mw'), $(int mh')) } |]

data HorizontalAlignment =
    AlignmentLeft
  | AlignmentHorizontalCenter
  | AlignmentRight
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

data VerticalAlignment =
    AlignmentTop
  | AlignmentVerticalCenter
  | AlignmentBottom
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Changes element alignment behavior
uiElementSetAlignment :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> HorizontalAlignment -- ^ Horizontal behavior
  -> VerticalAlignment -- ^ Vertical behaivor
  -> m ()
uiElementSetAlignment ptr ha va = liftIO $ do
  let ptr' = parentPointer ptr
      ha' = fromIntegral $ fromEnum ha
      va' = fromIntegral $ fromEnum va
  [C.exp| void { $(UIElement* ptr')->SetAlignment((HorizontalAlignment)$(int ha'), (VerticalAlignment)$(int va')) } |]

-- | Changes element alignment behavior
uiElementSetHorizontalAlignment :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> HorizontalAlignment -- ^ Horizontal behavior
  -> m ()
uiElementSetHorizontalAlignment ptr ha = liftIO $ do
  let ptr' = parentPointer ptr
      ha' = fromIntegral $ fromEnum ha
  [C.exp| void { $(UIElement* ptr')->SetHorizontalAlignment((HorizontalAlignment)$(int ha')) } |]

-- | Changes element alignment behavior
uiElementSetVerticalAlignment :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> VerticalAlignment -- ^ Vertical behaivor
  -> m ()
uiElementSetVerticalAlignment ptr  va = liftIO $ do
  let ptr' = parentPointer ptr
      va' = fromIntegral $ fromEnum va
  [C.exp| void { $(UIElement* ptr')->SetVerticalAlignment((VerticalAlignment)$(int va')) } |]

-- | Set child element clipping border
uiElementSetClipBorder :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> IntRect -- ^ border
  -> m ()
uiElementSetClipBorder p r = liftIO $ with r $ \r' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetClipBorder(*$(IntRect* r')) } |]

-- | Changes element opacity (inverse of alpha)
uiElementSetOpacity :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Float -- ^ Opacity
  -> m ()
uiElementSetOpacity ptr opacity = liftIO $ do
  let ptr' = parentPointer ptr
      opacity' = realToFrac $ opacity
  [C.exp| void { $(UIElement* ptr')->SetOpacity($(float opacity')) } |]

-- | Set whether should be brought to front when focused.
uiElementSetBringToFront :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetBringToFront p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetBringToFront($(int b') != 0) } |]

-- | Set whether should be put to background when another element is focused.
uiElementSetBringToBack :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetBringToBack p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetBringToBack($(int b') != 0) } |]

-- | Set whether should clip child elements. Default false.
uiElementSetClipChildren :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetClipChildren p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetClipChildren($(int b') != 0) } |]

-- | Set whether should sort child elements according to priority. Default true.
uiElementSetSortChildren :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetSortChildren p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetSortChildren($(int b') != 0) } |]

-- | Set whether parent elements' opacity affects opacity. Default true.
uiElementSetUseDerivedOpacity :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetUseDerivedOpacity p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetUseDerivedOpacity($(int b') != 0) } |]

-- | Set whether reacts to input. Default false, but is enabled by subclasses if applicable.
uiElementSetEnabled :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetEnabled p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetEnabled($(int b') != 0) } |]

-- | Set enabled state on self and child elements. Elements' own enabled state is remembered (IsEnabledSelf) and can be restored.
uiElementSetDeepEnabled :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetDeepEnabled p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetDeepEnabled($(int b') != 0) } |]

-- | Reset enabled state to the element's remembered state prior to calling SetDeepEnabled.
uiElementResetDeepEnabled :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> m ()
uiElementResetDeepEnabled p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->ResetDeepEnabled() } |]

-- | Set enabled state on self and child elements. Unlike SetDeepEnabled this does not remember the elements' own enabled state, but overwrites it.
uiElementSetEnabledRecursive :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetEnabledRecursive p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetEnabledRecursive($(int b') != 0) } |]

-- | Set whether value is editable through input. Not applicable to all elements. Default true.
uiElementSetEditable :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetEditable p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetEditable($(int b') != 0) } |]

-- | Set whether is focused. Only one element can be focused at a time.
uiElementSetFocus :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetFocus p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetFocus($(int b') != 0) } |]

-- | Set selected mode. Actual meaning is element dependent, for example constant hover or pressed effect.
uiElementSetSelected :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetSelected p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetSelected($(int b') != 0) } |]

-- | Set whether is visible. Visibility propagates to child elements.
uiElementSetVisible :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetVisible p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetVisible($(int b') != 0) } |]

-- | Mark as internally (programmatically) created. Used when an element composes itself out of child elements.
uiElementSetInternal :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetInternal p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetInternal($(int b') != 0) } |]

-- | Set element event sender flag. When child element is added or deleted, the event would be sent using UIElement found in the parental chain having this flag set. If not set, the event is sent using UI's root as per normal.
uiElementSetElementEventSender :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetElementEventSender p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetElementEventSender($(int b') != 0) } |]

-- | Set hovering state.
uiElementSetHovering :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Value
  -> m ()
uiElementSetHovering p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->SetHovering($(int b') != 0) } |]

-- | Set child offset.
uiElementSetChildOffset :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ Offset
  -> m ()
uiElementSetChildOffset p o = liftIO $ with o $ \o' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetChildOffset(*$(IntVector2* o')) } |]

-- | Adjust scissor for rendering.
uiElementAdjustScissor :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> IntRect -- ^ currentScissor
  -> m ()
uiElementAdjustScissor p s = liftIO $ with s $ \s' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->AdjustScissor(*$(IntRect* s')) } |]

-- | UI element focus mode.
data FocusMode =
    NotFocusable -- ^ Is not focusable and does not affect existing focus.
  | ResetFocus -- ^ Resets focus when clicked.
  | Focusable -- ^ Is focusable.
  | Focusable'Defocusable -- ^ Is focusable and also defocusable by pressing ESC.
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

-- | Set focus mode.
uiElementSetFocusMode :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> FocusMode -- ^ Value
  -> m ()
uiElementSetFocusMode p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromIntegral $ fromEnum b
  [C.exp| void { $(UIElement* ptr)->SetFocusMode((FocusMode)$(int b')) } |]

-- | Set drag and drop flags.
uiElementSetDragDropMode :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ mode (TODO: add typed enum)
  -> m ()
uiElementSetDragDropMode ptr p = liftIO $ do
  let ptr' = parentPointer ptr
      p' = fromIntegral p
  [C.exp| void { $(UIElement* ptr')->SetDragDropMode($(int p')) } |]

-- | Traversal mode for rendering.
data TraversalMode =
  -- | Traverse thru children having same priority first and recurse into their children before traversing children having higher priority.
    BreadthFirst
  -- | Traverse thru each child and its children immediately after in sequence.
  | DepthFirst
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

-- | Set traversal mode for rendering. The default traversal mode is TM_BREADTH_FIRST for non-root element. Root element should be set to TM_DEPTH_FIRST to avoid artifacts during rendering.
uiElementSetTraversalMode :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> TraversalMode -- ^ Value
  -> m ()
uiElementSetTraversalMode p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromIntegral $ fromEnum b
  [C.exp| void { $(UIElement* ptr)->SetTraversalMode((TraversalMode)$(int b')) } |]

-- | Set horizontal indentation.
uiElementSetIndent :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ ident level
  -> m ()
uiElementSetIndent ptr p = liftIO $ do
  let ptr' = parentPointer ptr
      p' = fromIntegral $ p
  [C.exp| void { $(UIElement* ptr')->SetIndent($(int p')) } |]

-- | Set indent spacing (number of pixels per indentation level).
uiElementSetIndentSpacing :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Number of pixels per identation level
  -> m ()
uiElementSetIndentSpacing ptr p = liftIO $ do
  let ptr' = parentPointer ptr
      p' = fromIntegral $ p
  [C.exp| void { $(UIElement* ptr')->SetIndentSpacing($(int p')) } |]

-- | Changes element priority (controls overlaying with other elements)
uiElementSetPriority :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Priority (could be negative)
  -> m ()
uiElementSetPriority ptr p = liftIO $ do
  let ptr' = parentPointer ptr
      p' = fromIntegral $ p
  [C.exp| void { $(UIElement* ptr')->SetPriority($(int p')) } |]

-- | Manually update layout. Should not be necessary in most cases, but is provided for completeness.
uiElementUpdateLayout :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> m ()
uiElementUpdateLayout p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->UpdateLayout() } |]

-- | Disable automatic layout update. Should only be used if there are performance problems.
uiElementDisableLayoutUpdate :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> m ()
uiElementDisableLayoutUpdate p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->DisableLayoutUpdate() } |]

-- | Enable automatic layout update.
uiElementEnableLayoutUpdate :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> m ()
uiElementEnableLayoutUpdate p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->EnableLayoutUpdate() } |]

-- | Bring UI element to front.
uiElementBringToFront :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> m ()
uiElementBringToFront p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->BringToFront() } |]

-- | Changes element position in screen coordinates
uiElementSetPosition :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ Position
  -> m ()
uiElementSetPosition p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetPosition(*$(IntVector2* v'))} |]

-- | Adds child to the element
uiElementAddChild :: (Parent UIElement a, Pointer p1 a, Parent UIElement b, Pointer p2 b, MonadIO m)
  => p1 -- ^ Pointer to UI element
  -> p2 -- ^ Pointer to child UIElement
  -> m ()
uiElementAddChild p1 p2 = liftIO $ do
  let ptr1 = parentPointer p1
      ptr2 = parentPointer p2
  [C.exp| void { $(UIElement* ptr1)->AddChild($(UIElement* ptr2)) } |]

-- | Insert a child element into a specific position in the child list.
uiElementInsertChild :: (Parent UIElement a, Pointer p1 a, Parent UIElement b, Pointer p2 b, MonadIO m)
  => p1 -- ^ Pointer to UI element
  -> Int -- ^ Index where to insert
  -> p2 -- ^ Pointer to child UIElement
  -> m ()
uiElementInsertChild p1 i p2 = liftIO $ do
  let ptr1 = parentPointer p1
      ptr2 = parentPointer p2
      i' = fromIntegral i
  [C.exp| void { $(UIElement* ptr1)->InsertChild($(int i'), $(UIElement* ptr2)) } |]

-- | Remove a child element. Starting search at specified index if provided.
uiElementRemoveChild :: (Parent UIElement a, Pointer p1 a, Parent UIElement b, Pointer p2 b, MonadIO m)
  => p1 -- ^ Pointer to UI element
  -> p2 -- ^ Pointer to child UIElement
  -> Int -- ^ Index where to start scan (default 0)
  -> m ()
uiElementRemoveChild p1 p2 i = liftIO $ do
  let ptr1 = parentPointer p1
      ptr2 = parentPointer p2
      i' = fromIntegral i
  [C.exp| void { $(UIElement* ptr1)->RemoveChild($(UIElement* ptr2), $(int i')) } |]

-- | Remove a child element at index.
uiElementRemoveChildAtIndex :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> Int -- ^ Index which should be removed
  -> m ()
uiElementRemoveChildAtIndex p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  [C.exp| void { $(UIElement* ptr)->RemoveChildAtIndex($(int i')) } |]

-- | Remove all child elements.
uiElementRemoveAllChildren :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> m ()
uiElementRemoveAllChildren p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->RemoveAllChildren() } |]

-- | Remove from the parent element. If no other shared pointer references exist, causes immediate deletion.
uiElementRemove :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> m ()
uiElementRemove p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->Remove() } |]

-- | Find child index. Return Nothing if not found.
uiElementFindChild :: (Parent UIElement a, Pointer p1 a, Parent UIElement b, Pointer p2 b, MonadIO m)
  => p1 -- ^ Pointer to UI element
  -> p2 -- ^ Pointer to child UIElement
  -> m (Maybe Int)
uiElementFindChild p1 p2 = liftIO $ do
  let ptr1 = parentPointer p1
      ptr2 = parentPointer p2
  i <- [C.exp| int { $(UIElement* ptr1)->FindChild($(UIElement* ptr2)) } |]
  return $ if i == [C.pure| int{M_MAX_UNSIGNED} |]
    then Nothing
    else Just $ fromIntegral i

-- | Set parent element. Same as parent->InsertChild(index, this).
uiElementSetParent :: (Parent UIElement a, Pointer p1 a, Parent UIElement b, Pointer p2 b, MonadIO m)
  => p1 -- ^ Pointer to UI element
  -> p2 -- ^ Pointer to child UIElement
  -> Maybe Int -- ^ insert index
  -> m ()
uiElementSetParent p1 p2 i = liftIO $ do
  let ptr1 = parentPointer p1
      ptr2 = parentPointer p2
      i' = maybe [C.pure| int {M_MAX_UNSIGNED} |] fromIntegral i
  [C.exp| void { $(UIElement* ptr1)->SetParent($(UIElement* ptr2), $(int i')) } |]

-- | Set a user variable.
uiElementSetVar :: (Parent UIElement b, Pointer p b, VariantStorable a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> String -- ^ Var name
  -> a -- ^ Value
  -> m ()
uiElementSetVar p name a = liftIO $ withObject name $ \name' -> withVariant a $ \a' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetVar(*$(StringHash* name'), *$(Variant* a')) } |]

-- | Set default style file for later use by children elements.
uiElementSetDefaultStyle :: (Parent UIElement a, Pointer p a, Parent XMLFile b, Pointer xmlFile b, MonadIO m)
  => p -- ^ Pointer to UI element
  -> xmlFile -- ^ Pointer to XMLFile that stores UI style info
  -> m ()
uiElementSetDefaultStyle p s = liftIO $ do
  let ptr = parentPointer p
      style = parentPointer s
  [C.exp| void { $(UIElement* ptr)->SetDefaultStyle($(XMLFile* style))} |]

-- | Set name.
uiElementSetName :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> String -- ^ Name
  -> m ()
uiElementSetName p s = liftIO $ withCString s $ \str -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetName($(const char* str)) } |]

-- | Set style from an XML file. Find the style element automatically.
-- If the style file is not explicitly provided, use the default style from parental chain.
-- Return true if the style is applied successfully.
uiElementSetStyleAuto :: (Parent UIElement a, Pointer p a, Parent XMLFile b, Pointer xmlFile b, MonadIO m)
  => p -- ^ Pointer to UI element
  -> xmlFile -- ^ Pointer to style file, or @nullPtr@ for defult sytle
  -> m Bool
uiElementSetStyleAuto p xml = liftIO $ do
  let ptr = parentPointer p
      xml' = parentPointer xml
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->SetStyleAuto($(XMLFile* xml')) } |]

-- | Set style from the default style from parental chain. Find the style element automatically.
-- Return true if the style is applied successfully.
uiElementSetStyleAutoDefault :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementSetStyleAutoDefault p = uiElementSetStyleAuto p (nullPtr :: Ptr XMLFile)

-- | Set style from an XML file. Find the style element by name.
-- If the style file is not explicitly provided, use the default style from parental chain.
-- Return true if the style is applied successfully.
uiElementSetStyle :: (Parent UIElement a, Pointer p a, Parent XMLFile b, Pointer xmlFile b, MonadIO m)
  => p -- ^ Pointer to UI element
  -> String -- ^ Style name
  -> xmlFile -- ^ xml file with style, if nullPtr then will find style in default settings
  -> m Bool
uiElementSetStyle p str xml = liftIO $ withCString str $ \str' -> do
  let ptr = parentPointer p
      xml' = parentPointer xml
  toBool <$> [C.exp| int { $(UIElement* ptr)->SetStyle(String($(const char* str')), $(XMLFile* xml')) } |]

-- | Set style from an XML element. Return true if the style is applied successfully.
uiElementSetStyle' :: (Parent UIElement a, Pointer p a, Parent XMLElement b, Pointer xmlElement b, MonadIO m)
  => p -- ^ Pointer to UI element
  -> xmlElement
  -> m Bool
uiElementSetStyle' p xml = liftIO $ do
  let ptr = parentPointer p
      xml' = parentPointer xml
  toBool <$> [C.exp| int { $(UIElement* ptr)->SetStyle(*$(XMLElement* xml')) } |]

-- | Set style from the default style from parental chain. Find the style element by name.
-- Return true if the style is applied successfully.
uiElementSetStyleDefault :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> String -- ^ Style name
  -> m Bool
uiElementSetStyleDefault p str = uiElementSetStyle p str (nullPtr :: Ptr XMLFile)

-- | Layout operation mode.
data LayoutMode =
    -- | No layout operations will be performed
    LayoutFree
    -- |  Layout child elements horizontally and resize them to fit. Resize element if necessary.
  | LayoutHorizontal
    -- | Layout child elements vertically and resize them to fit. Resize element if necessary.
  | LayoutVertical
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Generic)

-- | Set layout.
uiElementSetLayout :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> LayoutMode
  -> Int -- ^ spacing
  -> IntRect -- ^ border margin
  -> m ()
uiElementSetLayout p mode spacing border = liftIO $ with border $ \border' -> do
  let ptr = parentPointer p
      mode' = fromIntegral $ fromEnum mode
      spacing' = fromIntegral spacing
  [C.exp| void { $(UIElement* ptr)->SetLayout((LayoutMode)$(int mode'), $(int spacing'), *$(IntRect* border')) } |]

-- | Set layout mode only.
uiElementSetLayoutMode :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> LayoutMode
  -> m ()
uiElementSetLayoutMode p mode = liftIO $ do
  let ptr = parentPointer p
      mode' = fromIntegral $ fromEnum mode
  [C.exp| void { $(UIElement* ptr)->SetLayoutMode((LayoutMode)$(int mode')) } |]

-- | Set layout mode only.
uiElementSetLayoutSpacing :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> Int -- ^ spacing in pixels
  -> m ()
uiElementSetLayoutSpacing p spacing = liftIO $ do
  let ptr = parentPointer p
      spacing' = fromIntegral spacing
  [C.exp| void { $(UIElement* ptr)->SetLayoutSpacing($(int spacing')) } |]

-- | Set layout border only.
uiElementSetLayoutBorder :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> IntRect -- ^ border margin
  -> m ()
uiElementSetLayoutBorder p border = liftIO $ with border $ \border' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetLayoutBorder(*$(IntRect* border')) } |]

-- | Set layout flex scale.
uiElementSetLayoutFlexScale :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> Vector2 -- ^ scale
  -> m ()
uiElementSetLayoutFlexScale p scale = liftIO $ with scale $ \scale' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetLayoutFlexScale(*$(Vector2* scale')) } |]

-- | Set color on all corners.
uiElementSetColor :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> Color -- ^ color for all corners
  -> m ()
uiElementSetColor p color = liftIO $ with color $ \color' -> do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SetColor(*$(Color* color')) } |]

-- | Element corners.
data Corner =
    CornerTopLeft
  | CornerTopRight
  | CornerBottomLeft
  | CornerBottomRight
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Generic)

-- | Set color on all corners.
uiElementSetCornerColor :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> Corner -- ^ Which corner to modify
  -> Color -- ^ color for all corners
  -> m ()
uiElementSetCornerColor p corner color = liftIO $ with color $ \color' -> do
  let ptr = parentPointer p
      corner' = fromIntegral $ fromEnum corner
  [C.exp| void { $(UIElement* ptr)->SetColor((Corner)$(int corner'), *$(Color* color')) } |]

-- | Return size
uiElementGetSize :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m IntVector2
uiElementGetSize p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* { &$(UIElement* ptr)->GetSize() } |]

-- | Return width
uiElementGetWidth :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetWidth p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetWidth() } |]

-- | Return height
uiElementGetHeight :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetHeight p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetHeight() } |]

-- | Return position
uiElementGetPosition :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m IntVector2
uiElementGetPosition p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* { &$(UIElement* ptr)->GetPosition() } |]

-- | Return name
uiElementGetName :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m String
uiElementGetName p = liftIO $ do
  let ptr = parentPointer p
  loadConstUrhoString =<< [C.exp| const String* { &$(UIElement* ptr)->GetName() } |]

-- | Retrusn corner color
uiElementGetColor :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> Corner -- ^ Which corner color to get
  -> m Color
uiElementGetColor p corner = liftIO $ do
  let ptr = parentPointer p
      corner' = fromIntegral $ fromEnum corner
  peek =<< [C.exp| const Color* { &$(UIElement* ptr)->GetColor((Corner)$(int corner')) } |]

-- | Return minimum size.
uiElementGetMinSize :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m IntVector2
uiElementGetMinSize p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* { &$(UIElement* ptr)->GetMinSize() } |]

-- | Return minimum width.
uiElementGetMinWidth :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetMinWidth p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetMinWidth() } |]

-- | Return minimum width.
uiElementGetMinHeight :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetMinHeight p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetMinHeight() } |]

-- | Return maximum size.
uiElementGetMaxSize :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m IntVector2
uiElementGetMaxSize p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* { &$(UIElement* ptr)->GetMaxSize() } |]

-- | Return minimum width.
uiElementGetMaxWidth :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetMaxWidth p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetMaxWidth() } |]

-- | Return minimum width.
uiElementGetMaxHeight :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetMaxHeight p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetMaxHeight() } |]

-- | Return true if size is fixed.
uiElementIsFixedSize :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsFixedSize p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsFixedSize() } |]

-- | Return true if width is fixed.
uiElementIsFixedWidth :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsFixedWidth p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsFixedWidth() } |]

-- | Return true if height is fixed.
uiElementIsFixedHeight :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsFixedHeight p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsFixedHeight() } |]

-- | Return child element offset.
uiElementGetChildOffset :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m IntVector2
uiElementGetChildOffset p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* { &$(UIElement* ptr)->GetChildOffset() } |]

-- | Return horizontal alignment.
uiElementGetHorizontalAlignment :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m HorizontalAlignment
uiElementGetHorizontalAlignment p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int { (int)$(UIElement* ptr)->GetHorizontalAlignment() } |]

-- | Return vertical alignment.
uiElementGetVerticalAlignment :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m VerticalAlignment
uiElementGetVerticalAlignment p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int { (int)$(UIElement* ptr)->GetVerticalAlignment() } |]

-- | Return child element offset.
uiElementGetClipBorder :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m IntRect
uiElementGetClipBorder p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntRect* { &$(UIElement* ptr)->GetClipBorder() } |]

-- | Return priority.
uiElementGetPriority :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetPriority p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetPriority() } |]

-- | Return priority.
uiElementGetOpacity :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Float
uiElementGetOpacity p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(UIElement* ptr)->GetOpacity() } |]

-- | Return derived opacity (affected by parent elements.) If UseDerivedOpacity is false, returns same as element's own opacity.
uiElementGetDerivedOpacity :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Float
uiElementGetDerivedOpacity p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(UIElement* ptr)->GetDerivedOpacity() } |]

-- | Return whether should be brought to front when focused.
uiElementGetBringToFront :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementGetBringToFront p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->GetBringToFront() } |]

-- | Return whether should be put to background when another element is focused.
uiElementGetBringToBack :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementGetBringToBack p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->GetBringToBack() } |]

-- | Return whether should clip child elements.
uiElementGetClipChildren :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementGetClipChildren p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->GetClipChildren() } |]

-- | Return whether should sort child elements according to priority.
uiElementGetSortChildren :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementGetSortChildren p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->GetSortChildren() } |]

-- | Return whether parent elements' opacity affects opacity.
uiElementGetUseDerivedOpacity :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementGetUseDerivedOpacity p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->GetUseDerivedOpacity() } |]

-- | Return whether has focus.
uiElementHasFocus :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementHasFocus p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->HasFocus() } |]

-- | Return whether reacts to input.
uiElementIsEnabled :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsEnabled p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsEnabled() } |]

-- | Returns the element's last own enabled state. May be different than the value returned by IsEnabled when SetDeepEnabled has been used.
uiElementIsEnabledSelf :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsEnabledSelf p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsEnabledSelf() } |]

-- | Return whether value is editable through input.
uiElementIsEditable :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsEditable p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsEditable() } |]

-- | Return whether is selected. Actual meaning is element dependent.
uiElementIsSelected :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsSelected p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsSelected() } |]

-- | Return whether element itself should be visible. Elements can be also hidden due to the parent being not visible, use IsVisibleEffective() to check.
uiElementIsVisible :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsVisible p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsVisible() } |]

-- | Return whether element is effectively visible (parent element chain is visible.)
uiElementIsVisibleEffective :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsVisibleEffective p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsVisibleEffective() } |]

-- | Return whether the cursor is hovering on this element.
uiElementIsHovering :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsHovering p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsHovering() } |]

-- | Return whether is internally created.
uiElementIsInternal :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsInternal p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsInternal() } |]

-- | Return whether has different color in at least one corner.
uiElementHasColorGradient :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementHasColorGradient p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->HasColorGradient() } |]

-- | Return focus mode.
uiElementGetFocusMode :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m FocusMode
uiElementGetFocusMode p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int { (int)$(UIElement* ptr)->GetFocusMode() } |]

-- | Return drag and drop flags.
uiElementGetDragDropMode :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int -- ^ Flags, TODO: add typed enum
uiElementGetDragDropMode p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetDragDropMode() } |]

-- | Return applied style name. Return an empty string when the applied style is an 'auto' style (i.e. style derived from instance's type).
uiElementGetAppliedStyle :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m String
uiElementGetAppliedStyle p = liftIO $ do
  let ptr = parentPointer p
  peekCString =<< [C.exp| const char* { $(UIElement* ptr)->GetAppliedStyle().CString() } |]

-- | Return default style.
uiElementGetDefaultStyle :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> Bool -- ^ Recursive up. Default True
  -> m (Maybe (Ptr XMLFile))
uiElementGetDefaultStyle p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  ps <- [C.exp| XMLFile* { $(UIElement* ptr)->GetDefaultStyle($(int b') != 0) } |]
  checkNullPtr' ps return

-- | Return layout mode.
uiElementGetLayoutMode :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m LayoutMode
uiElementGetLayoutMode p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int { (int)$(UIElement* ptr)->GetLayoutMode() } |]

-- | Return layout spacing.
uiElementGetLayoutSpacing :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetLayoutSpacing p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetLayoutSpacing() } |]

-- | Return layout border.
uiElementGetLayoutBorder :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m IntRect
uiElementGetLayoutBorder p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntRect* { &$(UIElement* ptr)->GetLayoutBorder() } |]

-- | Return layout flex scale.
uiElementGetLayoutFlexScale :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Vector2
uiElementGetLayoutFlexScale p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Vector2* { &$(UIElement* ptr)->GetLayoutFlexScale() } |]

-- | Return number of child elements.
uiElementGetNumChildren :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> Bool -- ^ recursive?
  -> m Int
uiElementGetNumChildren p b = liftIO $ do
  let ptr = parentPointer p
      b' = fromBool b
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetNumChildren($(int b') != 0) } |]

-- | Return child element by index.
uiElementGetChildByIndex :: (Parent UIElement a, Pointer p a, Parent UIElement e, MonadIO m)
  => p -- ^ Pointer to UI element
  -> Int -- ^ index
  -> m (Maybe (Ptr e))
uiElementGetChildByIndex p i = liftIO $ do
  let ptr = parentPointer p
      i' = fromIntegral i
  pe <- [C.exp| UIElement* { $(UIElement* ptr)->GetChild($(unsigned int i')) } |]
  fmap join $ checkNullPtr' pe $ return . castToChild

-- | Return child element by name.
uiElementGetChildByName :: (Parent UIElement a, Pointer p a, Parent UIElement e, MonadIO m)
  => p -- ^ Pointer to UI element
  -> String -- ^ name
  -> Bool -- ^ recusive? Default false
  -> m (Maybe (Ptr e))
uiElementGetChildByName p name b = liftIO $ withCString name $ \name' -> do
  let ptr = parentPointer p
      b' = fromBool b
  pe <- [C.exp| UIElement* { $(UIElement* ptr)->GetChild(String($(const char* name')), $(int b') != 0) } |]
  fmap join $ checkNullPtr' pe $ return . castToChild

-- | Return child element by variable. If only key is provided, return the first child having the matching variable key. If value is also provided then the actual variable value would also be checked against.
uiElementGetChildByVariable :: (Parent UIElement b, Pointer p b, VariantStorable a, Parent UIElement e, MonadIO m)
  => p -- ^ Pointer to UI element
  -> String -- ^ key
  -> Maybe a -- ^ value of variable, if provided, filtering also with the value
  -> Bool -- ^ recusive? Default false
  -> m (Maybe (Ptr e))
uiElementGetChildByVariable p key ma b = liftIO $ withObject key $ \key' -> withVariantMaybe ma $ \a -> do
  let ptr = parentPointer p
      b' = fromBool b
  pe <- [C.exp| UIElement* { $(UIElement* ptr)->GetChild(*$(StringHash* key'), *$(Variant* a), $(int b') != 0) } |]
  fmap join $ checkNullPtr' pe $ return . castToChild

podVectorPtr "UIElement"

-- | Return child elements either recursively or non-recursively.
uiElementGetChildren :: (Parent UIElement a, Pointer p a, MonadIO m, ForeignVectorRepresent v, ForeignElemConstr v (Ptr UIElement))
  => p -- ^ Pointer to UI element
  -> Bool -- ^ recusive? Default false
  -> m (v (Ptr UIElement))
uiElementGetChildren p b = liftIO $ withObject () $ \v -> do
  let ptr = parentPointer p
      b' = fromBool b
  [C.exp| void { $(UIElement* ptr)->GetChildren(*$(PODVectorUIElementPtr* v), $(int b') != 0) } |]
  peekForeignVectorAs v

-- | Return parent element.
uiElementGetParent :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m (Maybe (Ptr UIElement))
uiElementGetParent p = liftIO $ do
  let ptr = parentPointer p
  pp <- [C.exp| UIElement* { $(UIElement* ptr)->GetParent() } |]
  checkNullPtr' pp return

-- | Return root element.
uiElementGetRoot :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m (Ptr UIElement)
uiElementGetRoot p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| UIElement* { $(UIElement* ptr)->GetRoot() } |]

-- | Return derived color. Only valid when no gradient.
uiElementGetDerivedColor :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Color
uiElementGetDerivedColor p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const Color* { &$(UIElement* ptr)->GetDerivedColor() } |]

-- | Return a user variable.
uiElementGetVar :: (Parent UIElement b, Pointer p b, VariantStorable a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> String -- ^ key
  -> m (Maybe a)
uiElementGetVar p key = liftIO $ withObject key $ \key' -> do
  let ptr = parentPointer p
  getVariant =<< [C.exp| const Variant* { &$(UIElement* ptr)->GetVar(*$(StringHash* key')) } |]

-- | Return all user variables.
uiElementGetVars :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m (Ptr VariantMap)
uiElementGetVars p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| const HashMapStringHashVariant* { &$(UIElement* ptr)->GetVars() } |]

-- | Return the drag button combo if this element is being dragged.
uiElementGetDragButtonCombo :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetDragButtonCombo p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetDragButtonCombo() } |]

-- | Return the number of buttons dragging this element.
uiElementGetDragButtonCount :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetDragButtonCount p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(UIElement* ptr)->GetDragButtonCount() } |]

-- | Convert screen coordinates to element coordinates.
uiElementScreenToElement :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ screen position
  -> m IntVector2 -- ^ element position
uiElementScreenToElement p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  vp <- [C.exp| IntVector2* { new IntVector2($(UIElement* ptr)->ScreenToElement(*$(IntVector2* v'))) } |]
  deleteAfter vp peek

-- | Convert element coordinates to screen coordinates.
uiElementElementToScreen :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ element position
  -> m IntVector2 -- ^ screen position
uiElementElementToScreen p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  vp <- [C.exp| IntVector2* { new IntVector2($(UIElement* ptr)->ElementToScreen(*$(IntVector2* v'))) } |]
  deleteAfter vp peek

-- | Return whether a point (either in element or screen coordinates) is inside the element.
uiElementIsInside :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ position
  -> Bool -- ^ is screen coordinate?
  -> m Bool
uiElementIsInside p v b = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
      b' = fromBool b
  toBool <$> [C.exp| int {$(UIElement* ptr)->IsInside(*$(IntVector2* v'), $(int b') != 0)} |]

-- | Return whether a point (either in element or screen coordinates) is inside the combined rect of the element and its children.
uiElementIsInsideCombined :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> IntVector2 -- ^ position
  -> Bool -- ^ is screen coordinate?
  -> m Bool
uiElementIsInsideCombined p v b = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
      b' = fromBool b
  toBool <$> [C.exp| int {$(UIElement* ptr)->IsInsideCombined(*$(IntVector2* v'), $(int b') != 0)} |]

-- | Return combined screen coordinate rect of element and its children.
uiElementGetCombinedScreenRect :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m IntRect
uiElementGetCombinedScreenRect p = liftIO $ do
  let ptr = parentPointer p
  rp <- [C.exp| IntRect* { new IntRect($(UIElement* ptr)->GetCombinedScreenRect()) }|]
  deleteAfter rp peek


-- | Sort child elements if sorting enabled and order dirty. Called by UI.
uiElementSortChildren :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m ()
uiElementSortChildren p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(UIElement* ptr)->SortChildren() } |]

-- | Return horizontal indentation.
uiElementGetIndent :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetIndent p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetIndent() } |]

-- | Return indent spacing (number of pixels per indentation level).
uiElementGetIndentSpacing :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetIndentSpacing p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetIndentSpacing() } |]

-- | Return indent width in pixels.
uiElementGetIndentWidth :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Int
uiElementGetIndentWidth p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| int { $(UIElement* ptr)->GetIndentWidth() } |]

-- | Return traversal mode for rendering.
uiElementGetTraversalMode :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m TraversalMode
uiElementGetTraversalMode p = liftIO $ do
  let ptr = parentPointer p
  toEnum . fromIntegral <$> [C.exp| int { (int)$(UIElement* ptr)->GetTraversalMode() } |]

-- | Return whether element should send child added / removed events by itself. If false, defers to parent element.
uiElementIsElementEventSender :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m Bool
uiElementIsElementEventSender p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { (int)$(UIElement* ptr)->IsElementEventSender() } |]

-- | Get element which should send child added / removed events.
uiElementGetElementEventSender :: (Parent UIElement a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to UI element
  -> m (Ptr UIElement)
uiElementGetElementEventSender p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| UIElement* { $(UIElement* ptr)->GetElementEventSender() } |]
