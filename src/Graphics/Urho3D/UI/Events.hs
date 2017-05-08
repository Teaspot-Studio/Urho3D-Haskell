{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.UI.Events(
    EventUIMouseClick(..)
  , EventUIMouseClickEnd(..)
  , EventUIMouseDoubleClick(..)
  , EventDragDropTest(..)
  , EventDragDropFinish(..)
  , EventFocusChanged(..)
  , EventNameChanged(..)
  , EventResized(..)
  , EventPositioned(..)
  , EventVisibleChanged(..)
  , EventDefocused(..)
  , EventLayoutUpdated(..)
  , EventPressed(..)
  , EventReleased(..)
  , EventSliderChanged(..)
  , EventSliderPaged(..)
  , EventScrollBarChanged(..)
  , EventViewChanged(..)
  , EventModalChanged(..)
  , EventTextEntry(..)
  , EventTextChanged(..)
  , EventTextFinished(..)
  , EventMenuSelected(..)
  , EventItemSelected(..)
  , EventItemDeselected(..)
  , EventSelectionChanged(..)
  , EventItemClicked(..)
  , EventItemDoubleClicked(..)
  , EventUnhandledKey(..)
  , EventFileSelected(..)
  , EventMessageACK(..)
  , EventElementAdded(..)
  , EventElementRemoved(..)
  , EventHoverBegin(..)
  , EventHoverEnd(..)
  , EventDragBegin(..)
  , EventDragMove(..)
  , EventDragEnd(..)
  , EventDragCancel(..)
  , EventUIDropFile(..)
  -- | Lenses API
  , HasElement(..)
  , HasBeginElement(..)
  , HasClickedElement(..)
  , HasX(..)
  , HasY(..)
  , HasButton(..)
  , HasButtons(..)
  , HasQualifiers(..)
  , HasSource(..)
  , HasTarget(..)
  , HasAccept(..)
  , HasWidth(..)
  , HasHeight(..)
  , HasVisible(..)
  , HasByKey(..)
  , HasState(..)
  , HasValue(..)
  , HasOffset(..)
  , HasPressed(..)
  , HasModal(..)
  , HasText(..)
  , HasSelection(..)
  , HasItem(..)
  , HasKey(..)
  , HasFileName(..)
  , HasFilter(..)
  , HasOk(..)
  , HasRoot(..)
  , HasParent(..)
  , HasElementX(..)
  , HasElementY(..)
  , HasNumButtons(..)
  , HasDx(..)
  , HasDy(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.Text as T

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Math.Vector2
import Data.Monoid
import Foreign
import Control.Lens

import Graphics.Urho3D.Graphics.Internal.Defs
import Graphics.Urho3D.Input.Internal.Input

C.context (C.cppCtx <> stringHashContext)
C.include "<Urho3D/UI/UIEvents.h>"
C.using "namespace Urho3D"

-- | Mouse click in the UI.
data EventUIMouseClick = EventUIMouseClick {
    _eventUIMouseClickElement :: Ptr UIElement
  , _eventUIMouseClickX :: Int
  , _eventUIMouseClickY :: Int
  , _eventUIMouseClickButton :: Int -- TODO: Special types
  , _eventUIMouseClickButtons :: Int
  , _eventUIMouseClickQualifiers :: Int
  } deriving (Show)
makeFields ''EventUIMouseClick

instance Event EventUIMouseClick where
  eventID _ = [C.pure| const StringHash* {&E_UIMOUSECLICK} |]
  loadEventData vmap = EventUIMouseClick
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&UIMouseClick::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClick::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClick::P_Y} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClick::P_BUTTON} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClick::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClick::P_QUALIFIERS} |]

-- | Mouse click end in the UI.
data EventUIMouseClickEnd = EventUIMouseClickEnd {
    _eventUIMouseClickEndElement :: Ptr UIElement
  , _eventUIMouseClickEndBeginElement :: Ptr UIElement
  , _eventUIMouseClickEndX :: Int
  , _eventUIMouseClickEndY :: Int
  , _eventUIMouseClickEndButton :: Int -- TODO: Special types
  , _eventUIMouseClickEndButtons :: Int
  , _eventUIMouseClickEndQualifiers :: Int
  } deriving (Show)
makeFields ''EventUIMouseClickEnd

instance Event EventUIMouseClickEnd where
  eventID _ = [C.pure| const StringHash* {&E_UIMOUSECLICKEND} |]
  loadEventData vmap = EventUIMouseClickEnd
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&UIMouseClickEnd::P_ELEMENT} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&UIMouseClickEnd::P_BEGINELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClickEnd::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClickEnd::P_Y} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClickEnd::P_BUTTON} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClickEnd::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseClickEnd::P_QUALIFIERS} |]

-- | Mouse double click in the UI.
data EventUIMouseDoubleClick = EventUIMouseDoubleClick {
    _eventUIMouseDoubleClickElement :: Ptr UIElement
  , _eventUIMouseDoubleClickX :: Int
  , _eventUIMouseDoubleClickY :: Int
  , _eventUIMouseDoubleClickButton :: Int -- TODO: Special types
  , _eventUIMouseDoubleClickButtons :: Int
  , _eventUIMouseDoubleClickQualifiers :: Int
  } deriving (Show)
makeFields ''EventUIMouseDoubleClick

instance Event EventUIMouseDoubleClick where
  eventID _ = [C.pure| const StringHash* {&E_UIMOUSEDOUBLECLICK} |]
  loadEventData vmap = EventUIMouseDoubleClick
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&UIMouseDoubleClick::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseDoubleClick::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseDoubleClick::P_Y} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseDoubleClick::P_BUTTON} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseDoubleClick::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIMouseDoubleClick::P_QUALIFIERS} |]

-- | Drag and drop test.
data EventDragDropTest = EventDragDropTest {
    _eventDragDropTestSource :: Ptr UIElement
  , _eventDragDropTestTarget :: Ptr UIElement
  , _eventDragDropTestAccept :: Bool
  } deriving (Show)
makeFields ''EventDragDropTest

instance Event EventDragDropTest where
  eventID _ = [C.pure| const StringHash* {&E_DRAGDROPTEST} |]
  loadEventData vmap = EventDragDropTest
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&DragDropTest::P_SOURCE} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&DragDropTest::P_TARGET} |]
    <*> variantMapGetDefault vmap False [C.pure| const StringHash* {&DragDropTest::P_ACCEPT} |]

-- | Drag and drop finish.
data EventDragDropFinish = EventDragDropFinish {
    _eventDragDropFinishSource :: Ptr UIElement
  , _eventDragDropFinishTarget :: Ptr UIElement
  , _eventDragDropFinishAccept :: Bool
  } deriving (Show)
makeFields ''EventDragDropFinish

instance Event EventDragDropFinish where
  eventID _ = [C.pure| const StringHash* {&E_DRAGDROPFINISH} |]
  loadEventData vmap = EventDragDropFinish
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&DragDropFinish::P_SOURCE} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&DragDropFinish::P_TARGET} |]
    <*> variantMapGetDefault vmap False [C.pure| const StringHash* {&DragDropFinish::P_ACCEPT} |]

-- | Focus element changed.
data EventFocusChanged = EventFocusChanged {
    _eventFocusChangedElement :: Ptr UIElement
  , _eventFocusChangedClickedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventFocusChanged

instance Event EventFocusChanged where
  eventID _ = [C.pure| const StringHash* {&E_FOCUSCHANGED} |]
  loadEventData vmap = EventFocusChanged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&FocusChanged::P_ELEMENT} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&FocusChanged::P_CLICKEDELEMENT} |]

-- | UI element name changed.
data EventNameChanged = EventNameChanged {
    _eventNameChangedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventNameChanged

instance Event EventNameChanged where
  eventID _ = [C.pure| const StringHash* {&E_NAMECHANGED} |]
  loadEventData vmap = EventNameChanged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&NameChanged::P_ELEMENT} |]

-- | UI element resized.
data EventResized = EventResized {
    _eventResizedElement :: Ptr UIElement
  , _eventResizedWidth :: Int
  , _eventResizedHeight :: Int
  } deriving (Show)
makeFields ''EventResized

instance Event EventResized where
  eventID _ = [C.pure| const StringHash* {&E_RESIZED} |]
  loadEventData vmap = EventResized
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&Resized::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&Resized::P_WIDTH} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&Resized::P_HEIGHT} |]

-- | UI element positioned.
data EventPositioned = EventPositioned {
    _eventPositionedElement :: Ptr UIElement
  , _eventPositionedX :: Int
  , _eventPositionedY :: Int
  } deriving (Show)
makeFields ''EventPositioned

instance Event EventPositioned where
  eventID _ = [C.pure| const StringHash* {&E_POSITIONED} |]
  loadEventData vmap = EventPositioned
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&Positioned::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&Positioned::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&Positioned::P_Y} |]

-- | UI element visibility changed.
data EventVisibleChanged = EventVisibleChanged {
    _eventVisibleChangedElement :: Ptr UIElement
  , _eventVisibleChangedVisible :: Bool
  } deriving (Show)
makeFields ''EventVisibleChanged

instance Event EventVisibleChanged where
  eventID _ = [C.pure| const StringHash* {&E_VISIBLECHANGED} |]
  loadEventData vmap = EventVisibleChanged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&VisibleChanged::P_ELEMENT} |]
    <*> variantMapGetDefault vmap False [C.pure| const StringHash* {&VisibleChanged::P_VISIBLE} |]

-- | UI element focused.
data EventFocused = EventFocused {
    _eventFocusedElement :: Ptr UIElement
  , _eventFocusedByKey :: Bool
  } deriving (Show)
makeFields ''EventFocused

instance Event EventFocused where
  eventID _ = [C.pure| const StringHash* {&E_FOCUSED} |]
  loadEventData vmap = EventFocused
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&Focused::P_ELEMENT} |]
    <*> variantMapGetDefault vmap False [C.pure| const StringHash* {&Focused::P_BYKEY} |]

-- | UI element defocused.
data EventDefocused = EventDefocused {
    _eventDefocusedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventDefocused

instance Event EventDefocused where
  eventID _ = [C.pure| const StringHash* {&E_DEFOCUSED} |]
  loadEventData vmap = EventDefocused
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&Defocused::P_ELEMENT} |]

-- | UI element layout updated.
data EventLayoutUpdated = EventLayoutUpdated {
    _eventLayoutUpdatedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventLayoutUpdated

instance Event EventLayoutUpdated where
  eventID _ = [C.pure| const StringHash* {&E_LAYOUTUPDATED} |]
  loadEventData vmap = EventLayoutUpdated
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&LayoutUpdated::P_ELEMENT} |]

-- | UI button pressed.
data EventPressed = EventPressed {
    _eventPressedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventPressed

instance Event EventPressed where
  eventID _ = [C.pure| const StringHash* {&E_PRESSED} |]
  loadEventData vmap = EventPressed
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&Pressed::P_ELEMENT} |]

-- | UI button was pressed, then released.
data EventReleased = EventReleased {
    _eventReleasedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventReleased

instance Event EventReleased where
  eventID _ = [C.pure| const StringHash* {&E_RELEASED} |]
  loadEventData vmap = EventReleased
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&Released::P_ELEMENT} |]

-- | UI checkbox toggled.
data EventToggled = EventToggled {
    _eventToggledElement :: Ptr UIElement
  , _eventToggledState :: Bool
  } deriving (Show)
makeFields ''EventToggled

instance Event EventToggled where
  eventID _ = [C.pure| const StringHash* {&E_TOGGLED} |]
  loadEventData vmap = EventToggled
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&Toggled::P_ELEMENT} |]
    <*> variantMapGetDefault vmap False [C.pure| const StringHash* {&Toggled::P_STATE} |]

-- | UI slider value changed
data EventSliderChanged = EventSliderChanged {
    _eventSliderChangedElement :: Ptr UIElement
  , _eventSliderChangedValue :: Float
  } deriving (Show)
makeFields ''EventSliderChanged

instance Event EventSliderChanged where
  eventID _ = [C.pure| const StringHash* {&E_SLIDERCHANGED} |]
  loadEventData vmap = EventSliderChanged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&SliderChanged::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&SliderChanged::P_VALUE} |]

-- | UI slider being paged.
data EventSliderPaged = EventSliderPaged {
    _eventSliderPagedElement :: Ptr UIElement
  , _eventSliderPagedOffset :: Int
  , _eventSliderPagedPressed :: Bool
  } deriving (Show)
makeFields ''EventSliderPaged

instance Event EventSliderPaged where
  eventID _ = [C.pure| const StringHash* {&E_SLIDERPAGED} |]
  loadEventData vmap = EventSliderPaged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&SliderPaged::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&SliderPaged::P_OFFSET} |]
    <*> variantMapGetDefault vmap False [C.pure| const StringHash* {&SliderPaged::P_PRESSED} |]

-- | UI scrollbar value changed.
data EventScrollBarChanged = EventScrollBarChanged {
    _eventScrollBarChangedElement :: Ptr UIElement
  , _eventScrollBarChangedValue :: Float
  } deriving (Show)
makeFields ''EventScrollBarChanged

instance Event EventScrollBarChanged where
  eventID _ = [C.pure| const StringHash* {&E_SCROLLBARCHANGED} |]
  loadEventData vmap = EventScrollBarChanged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ScrollBarChanged::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ScrollBarChanged::P_VALUE} |]

-- | UI scrollview position changed.
data EventViewChanged = EventViewChanged {
    _eventViewChangedElement :: Ptr UIElement
  , _eventViewChangedX :: Int
  , _eventViewChangedY :: Int
  } deriving (Show)
makeFields ''EventViewChanged

instance Event EventViewChanged where
  eventID _ = [C.pure| const StringHash* {&E_VIEWCHANGED} |]
  loadEventData vmap = EventViewChanged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ViewChanged::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ViewChanged::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ViewChanged::P_Y} |]

-- |  UI modal changed (currently only Window has modal flag).
data EventModalChanged = EventModalChanged {
    _eventModalChangedElement :: Ptr UIElement
  , _eventModalChangedModal :: Bool
  } deriving (Show)
makeFields ''EventModalChanged

instance Event EventModalChanged where
  eventID _ = [C.pure| const StringHash* {&E_MODALCHANGED} |]
  loadEventData vmap = EventModalChanged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ModalChanged::P_ELEMENT} |]
    <*> variantMapGetDefault vmap False [C.pure| const StringHash* {&ModalChanged::P_MODAL} |]

-- | Text entry into a LineEdit. The char can be modified in the event data.
data EventTextEntry = EventTextEntry {
    _eventTextEntryElement :: Ptr UIElement
  , _eventTextEntryText    :: T.Text
  } deriving (Show)
makeFields ''EventTextEntry

instance Event EventTextEntry where
  eventID _ = [C.pure| const StringHash* {&E_TEXTENTRY} |]
  loadEventData vmap = EventTextEntry
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&TextEntry::P_ELEMENT} |]
    <*> variantMapGetDefault vmap "" [C.pure| const StringHash* {&TextEntry::P_TEXT} |]

-- | Editable text changed
data EventTextChanged = EventTextChanged {
    _eventTextChangedElement :: Ptr UIElement
  , _eventTextChangedText :: T.Text
  } deriving (Show)
makeFields ''EventTextChanged

instance Event EventTextChanged where
  eventID _ = [C.pure| const StringHash* {&E_TEXTCHANGED} |]
  loadEventData vmap = EventTextChanged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&TextChanged::P_ELEMENT} |]
    <*> variantMapGetDefault vmap "" [C.pure| const StringHash* {&TextChanged::P_TEXT} |]

-- | Text editing finished (enter pressed on a LineEdit)
data EventTextFinished = EventTextFinished {
    _eventTextFinishedElement :: Ptr UIElement
  , _eventTextFinishedText :: T.Text
  , _eventTextFinishedValue :: Float
  } deriving (Show)
makeFields ''EventTextFinished

instance Event EventTextFinished where
  eventID _ = [C.pure| const StringHash* {&E_TEXTFINISHED} |]
  loadEventData vmap = EventTextFinished
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&TextFinished::P_ELEMENT} |]
    <*> variantMapGetDefault vmap "" [C.pure| const StringHash* {&TextFinished::P_TEXT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&TextFinished::P_VALUE} |]

-- | Menu selected.
data EventMenuSelected = EventMenuSelected {
    _eventMenuSelectedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventMenuSelected

instance Event EventMenuSelected where
  eventID _ = [C.pure| const StringHash* {&E_MENUSELECTED} |]
  loadEventData vmap = EventMenuSelected
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&MenuSelected::P_ELEMENT} |]

-- | Listview or DropDownList item selected.
data EventItemSelected = EventItemSelected {
    _eventItemSelectedElement :: Ptr UIElement
  , _eventItemSelectedSelection :: Int
  } deriving (Show)
makeFields ''EventItemSelected

instance Event EventItemSelected where
  eventID _ = [C.pure| const StringHash* {&E_ITEMSELECTED} |]
  loadEventData vmap = EventItemSelected
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ItemSelected::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemSelected::P_SELECTION} |]

-- | Listview item deselected.
data EventItemDeselected = EventItemDeselected {
    _eventItemDeselectedElement :: Ptr UIElement
  , _eventItemDeselectedSelection :: Int
  } deriving (Show)
makeFields ''EventItemDeselected

instance Event EventItemDeselected where
  eventID _ = [C.pure| const StringHash* {&E_ITEMDESELECTED} |]
  loadEventData vmap = EventItemDeselected
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ItemDeselected::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemDeselected::P_SELECTION} |]

-- | Listview selection change finished.
data EventSelectionChanged = EventSelectionChanged {
    _eventSelectionChangedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventSelectionChanged

instance Event EventSelectionChanged where
  eventID _ = [C.pure| const StringHash* {&E_SELECTIONCHANGED} |]
  loadEventData vmap = EventSelectionChanged
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&SelectionChanged::P_ELEMENT} |]

-- | Listview item clicked. If this is a left-click, also ItemSelected event will be sent. If this is a right-click, only this event is sent.
data EventItemClicked = EventItemClicked {
    _eventItemClickedElement :: Ptr UIElement
  , _eventItemClickedItem :: Ptr UIElement
  , _eventItemClickedSelection :: Int
  , _eventItemClickedButton :: Int -- TODO: Special types
  , _eventItemClickedButtons :: Int
  , _eventItemClickedQualifiers :: Int
  } deriving (Show)
makeFields ''EventItemClicked

instance Event EventItemClicked where
  eventID _ = [C.pure| const StringHash* {&E_ITEMCLICKED} |]
  loadEventData vmap = EventItemClicked
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ItemClicked::P_ELEMENT} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ItemClicked::P_ITEM} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemClicked::P_SELECTION} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemClicked::P_BUTTON} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemClicked::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemClicked::P_QUALIFIERS} |]

-- | Listview item double clicked.
data EventItemDoubleClicked = EventItemDoubleClicked {
    _eventItemDoubleClickedElement :: Ptr UIElement
  , _eventItemDoubleClickedItem :: Ptr UIElement
  , _eventItemDoubleClickedSelection :: Int
  , _eventItemDoubleClickedButton :: Int -- TODO: Special types
  , _eventItemDoubleClickedButtons :: Int
  , _eventItemDoubleClickedQualifiers :: Int
  } deriving (Show)
makeFields ''EventItemDoubleClicked

instance Event EventItemDoubleClicked where
  eventID _ = [C.pure| const StringHash* {&E_ITEMDOUBLECLICKED} |]
  loadEventData vmap = EventItemDoubleClicked
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ItemDoubleClicked::P_ELEMENT} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ItemDoubleClicked::P_ITEM} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemDoubleClicked::P_SELECTION} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemDoubleClicked::P_BUTTON} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemDoubleClicked::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&ItemDoubleClicked::P_QUALIFIERS} |]

-- | LineEdit or ListView unhandled key pressed.
data EventUnhandledKey = EventUnhandledKey {
    _eventUnhandledKeyElement :: Ptr UIElement
  , _eventUnhandledKeyKey :: Int -- TODO: Special types
  , _eventUnhandledKeyButtons :: Int
  , _eventUnhandledKeyQualifiers :: Int
  } deriving (Show)
makeFields ''EventUnhandledKey

instance Event EventUnhandledKey where
  eventID _ = [C.pure| const StringHash* {&E_UNHANDLEDKEY} |]
  loadEventData vmap = EventUnhandledKey
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&UnhandledKey::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UnhandledKey::P_KEY} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UnhandledKey::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UnhandledKey::P_QUALIFIERS} |]

-- | Fileselector choice.
data EventFileSelected = EventFileSelected {
    _eventFileSelectedFileName :: String
  , _eventFileSelectedFilter :: String
  , _eventFileSelectedOk :: Bool
  } deriving (Show)
makeFields ''EventFileSelected

instance Event EventFileSelected where
  eventID _ = [C.pure| const StringHash* {&E_FILESELECTED} |]
  loadEventData vmap = EventFileSelected
    <$> variantMapGetDefault vmap "" [C.pure| const StringHash* {&FileSelected::P_FILENAME} |]
    <*> variantMapGetDefault vmap "" [C.pure| const StringHash* {&FileSelected::P_FILTER} |]
    <*> variantMapGetDefault vmap False [C.pure| const StringHash* {&FileSelected::P_OK} |]

-- | MessageBox acknowlegement.
data EventMessageACK = EventMessageACK {
    _eventMessageACKOk :: Bool
  } deriving (Show)
makeFields ''EventMessageACK

instance Event EventMessageACK where
  eventID _ = [C.pure| const StringHash* {&E_MESSAGEACK} |]
  loadEventData vmap = EventMessageACK
    <$> variantMapGetDefault vmap False [C.pure| const StringHash* {&FileSelected::P_OK} |]

-- | A child element has been added to an element. Sent by the UI root element, or element-event-sender if set.
data EventElementAdded = EventElementAdded {
    _eventElementAddedRoot :: Ptr UIElement
  , _eventElementAddedParent :: Ptr UIElement
  , _eventElementAddedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventElementAdded

instance Event EventElementAdded where
  eventID _ = [C.pure| const StringHash* {&E_ELEMENTADDED} |]
  loadEventData vmap = EventElementAdded
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ElementAdded::P_ROOT} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ElementAdded::P_PARENT} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ElementAdded::P_ELEMENT} |]

-- | A child element has been added to an element. Sent by the UI root element, or element-event-sender if set.
data EventElementRemoved = EventElementRemoved {
    _eventElementRemovedRoot :: Ptr UIElement
  , _eventElementRemovedParent :: Ptr UIElement
  , _eventElementRemovedElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventElementRemoved

instance Event EventElementRemoved where
  eventID _ = [C.pure| const StringHash* {&E_ELEMENTREMOVED} |]
  loadEventData vmap = EventElementRemoved
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ElementRemoved::P_ROOT} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ElementRemoved::P_PARENT} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&ElementRemoved::P_ELEMENT} |]

-- | Hovering on an UI element has started
data EventHoverBegin = EventHoverBegin {
    _eventHoverBeginElement :: Ptr UIElement
  , _eventHoverBeginX :: Int
  , _eventHoverBeginY :: Int
  , _eventHoverBeginElementX :: Int
  , _eventHoverBeginElementY :: Int
  } deriving (Show)
makeFields ''EventHoverBegin

instance Event EventHoverBegin where
  eventID _ = [C.pure| const StringHash* {&E_HOVERBEGIN} |]
  loadEventData vmap = EventHoverBegin
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&HoverBegin::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&HoverBegin::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&HoverBegin::P_Y} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&HoverBegin::P_ELEMENTX} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&HoverBegin::P_ELEMENTY} |]

-- | Hovering on an UI element has ended
data EventHoverEnd = EventHoverEnd {
    _eventHoverEndElement :: Ptr UIElement
  } deriving (Show)
makeFields ''EventHoverEnd

instance Event EventHoverEnd where
  eventID _ = [C.pure| const StringHash* {&E_HOVEREND} |]
  loadEventData vmap = EventHoverEnd
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&HoverEnd::P_ELEMENT} |]

-- | Drag behavior of a UI Element has started
data EventDragBegin = EventDragBegin {
    _eventDragBeginElement :: Ptr UIElement
  , _eventDragBeginX :: Int
  , _eventDragBeginY :: Int
  , _eventDragBeginElementX :: Int
  , _eventDragBeginElementY :: Int
  , _eventDragBeginButtons :: Int  -- TODO: more specific types
  , _eventDragBeginNumButtons :: Int
  } deriving (Show)
makeFields ''EventDragBegin

instance Event EventDragBegin where
  eventID _ = [C.pure| const StringHash* {&E_DRAGBEGIN} |]
  loadEventData vmap = EventDragBegin
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&DragBegin::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragBegin::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragBegin::P_Y} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragBegin::P_ELEMENTX} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragBegin::P_ELEMENTY} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragBegin::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragBegin::P_NUMBUTTONS} |]

-- | Drag behavior of a UI Element when the input device has moved
data EventDragMove = EventDragMove {
    _eventDragMoveElement :: Ptr UIElement
  , _eventDragMoveX :: Int
  , _eventDragMoveY :: Int
  , _eventDragMoveDx :: Int
  , _eventDragMoveDy :: Int
  , _eventDragMoveElementX :: Int
  , _eventDragMoveElementY :: Int
  , _eventDragMoveButtons :: Int  -- TODO: more specific types
  , _eventDragMoveNumButtons :: Int
  } deriving (Show)
makeFields ''EventDragMove

instance Event EventDragMove where
  eventID _ = [C.pure| const StringHash* {&E_DRAGMOVE} |]
  loadEventData vmap = EventDragMove
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&DragMove::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragMove::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragMove::P_Y} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragMove::P_DX} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragMove::P_DY} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragMove::P_ELEMENTX} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragMove::P_ELEMENTY} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragMove::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragMove::P_NUMBUTTONS} |]

-- | Drag behavior of a UI Element has finished
data EventDragEnd = EventDragEnd {
    _eventDragEndElement :: Ptr UIElement
  , _eventDragEndX :: Int
  , _eventDragEndY :: Int
  , _eventDragEndElementX :: Int
  , _eventDragEndElementY :: Int
  , _eventDragEndButtons :: Int  -- TODO: more specific types
  , _eventDragEndNumButtons :: Int
  } deriving (Show)
makeFields ''EventDragEnd

instance Event EventDragEnd where
  eventID _ = [C.pure| const StringHash* {&E_DRAGEND} |]
  loadEventData vmap = EventDragEnd
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&DragEnd::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragEnd::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragEnd::P_Y} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragEnd::P_ELEMENTX} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragEnd::P_ELEMENTY} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragEnd::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragEnd::P_NUMBUTTONS} |]

-- | Drag of a UI Element was canceled by pressing ESC
data EventDragCancel = EventDragCancel {
    _eventDragCancelElement :: Ptr UIElement
  , _eventDragCancelX :: Int
  , _eventDragCancelY :: Int
  , _eventDragCancelElementX :: Int
  , _eventDragCancelElementY :: Int
  , _eventDragCancelButtons :: Int  -- TODO: more specific types
  , _eventDragCancelNumButtons :: Int
  } deriving (Show)
makeFields ''EventDragCancel

instance Event EventDragCancel where
  eventID _ = [C.pure| const StringHash* {&E_DRAGCANCEL} |]
  loadEventData vmap = EventDragCancel
    <$> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&DragCancel::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragCancel::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragCancel::P_Y} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragCancel::P_ELEMENTX} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragCancel::P_ELEMENTY} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragCancel::P_BUTTONS} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&DragCancel::P_NUMBUTTONS} |]

-- | A file was drag-dropped into the application window. Includes also coordinates and UI element if applicable
data EventUIDropFile = EventUIDropFile {
    _eventUIDropFileFileName :: String
  , _eventUIDropFileElement :: Ptr UIElement
  , _eventUIDropFileX :: Int
  , _eventUIDropFileY :: Int
  , _eventUIDropFileElementX :: Int
  , _eventUIDropFileElementY :: Int
  } deriving (Show)
makeFields ''EventUIDropFile

instance Event EventUIDropFile where
  eventID _ = [C.pure| const StringHash* {&E_UIDROPFILE} |]
  loadEventData vmap = EventUIDropFile
    <$> variantMapGetDefault vmap "" [C.pure| const StringHash* {&UIDropFile::P_FILENAME} |]
    <*> variantMapGetDefault vmap nullPtr [C.pure| const StringHash* {&UIDropFile::P_ELEMENT} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIDropFile::P_X} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIDropFile::P_Y} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIDropFile::P_ELEMENTX} |]
    <*> variantMapGetDefault vmap 0 [C.pure| const StringHash* {&UIDropFile::P_ELEMENTY} |]
