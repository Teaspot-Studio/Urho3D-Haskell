{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.ScrollView(
    ScrollView
  , scrollViewContext
  , SharedScrollView
  , scrollViewSetContentElement
  , scrollViewSetViewPosition
  , scrollViewSetScrollBarsVisible
  , scrollViewSetScrollBarsAutoVisible
  , scrollViewSetScrollStep
  , scrollViewSetPageStep
  , scrollViewSetScrollDeceleration
  , scrollViewSetScrollSnapEpsilon
  , scrollViewSetAutoDisableChildren
  , scrollViewSetAutoDisableThreshold
  , scrollViewGetViewPosition
  , scrollViewGetContentElement
  , scrollViewGetHorizontalScrollBar
  , scrollViewGetVerticalScrollBar
  , scrollViewGetScrollPanel
  , scrollViewGetScrollBarsAutoVisible
  , scrollViewGetScrollStep
  , scrollViewGetPageStep
  , scrollViewGetScrollDeceleration
  , scrollViewGetScrollSnapEpsilon
  , scrollViewGetAutoDisableChildren
  , scrollViewGetAutoDisableThreshold
  , scrollViewSetViewPositionAttr
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.ScrollView
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.UI.ScrollBar

C.context (C.cppCtx
  <> sharedScrollViewPtrCntx
  <> scrollViewCntx
  <> contextContext
  <> uiElementContext
  <> borderImageContext
  <> objectContext
  <> animatableContext
  <> serializableContext
  <> vector2Context
  <> scrollBarContext
  )
C.include "<Urho3D/UI/ScrollView.h>"
C.using "namespace Urho3D"

scrollViewContext :: C.Context
scrollViewContext = sharedScrollViewPtrCntx
  <> scrollViewCntx

instance Creatable (Ptr ScrollView) where
  type CreationOptions (Ptr ScrollView) = Ptr Context

  newObject ptr = liftIO $ [C.exp| ScrollView* { new ScrollView( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(ScrollView* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement] ''ScrollView

instance UIElem ScrollView where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { ScrollView::GetTypeStatic().Value() } |]

sharedPtr "ScrollView"

-- | Set content element.
-- void SetContentElement(UIElement* element);
scrollViewSetContentElement :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> Ptr UIElement
  -> m ()
scrollViewSetContentElement p v = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void {$(ScrollView* ptr)->SetContentElement($(UIElement* v))} |]

-- | Set view offset from the top-left corner.
-- void SetViewPosition(const IntVector2& position);
scrollViewSetViewPosition :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> IntVector2
  -> m ()
scrollViewSetViewPosition p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(ScrollView* ptr)->SetViewPosition(*$(IntVector2* v'))} |]

-- | Set scrollbars' visibility manually. Disables scrollbar autoshow/hide.
-- void SetScrollBarsVisible(bool horizontal, bool vertical);
scrollViewSetScrollBarsVisible :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> Bool -- ^ Horizontal
  -> Bool -- ^ Vertical
  -> m ()
scrollViewSetScrollBarsVisible p v1 v2  = liftIO $ do
  let ptr = parentPointer p
      v1' = fromBool v1
      v2' = fromBool v2
  [C.exp| void {$(ScrollView* ptr)->SetScrollBarsVisible($(int v1') != 0, $(int v2') != 0)} |]

-- | Set whether to automatically show/hide scrollbars. Default true.
-- void SetScrollBarsAutoVisible(bool enable);
scrollViewSetScrollBarsAutoVisible :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> Bool
  -> m ()
scrollViewSetScrollBarsAutoVisible p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(ScrollView* ptr)->SetScrollBarsAutoVisible($(int v') != 0)} |]

-- | Set arrow key scroll step. Also sets it on the scrollbars.
-- void SetScrollStep(float step);
scrollViewSetScrollStep :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> Float -- ^ step
  -> m ()
scrollViewSetScrollStep p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollView* ptr)->SetScrollStep($(float v'))} |]

-- | Set arrow key page step.
-- void SetPageStep(float step);
scrollViewSetPageStep :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> Float -- ^ step
  -> m ()
scrollViewSetPageStep p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollView* ptr)->SetPageStep($(float v'))} |]

-- | Set scroll deceleration.
-- void SetScrollDeceleration(float deceleration) { scrollDeceleration_ = deceleration; }
scrollViewSetScrollDeceleration :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> Float -- ^ deceleration
  -> m ()
scrollViewSetScrollDeceleration p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollView* ptr)->SetScrollDeceleration($(float v'))} |]

-- | Set scroll snap epsilon
-- void SetScrollSnapEpsilon(float snap) { scrollSnapEpsilon_ = snap; }
scrollViewSetScrollSnapEpsilon :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> Float -- ^ snap
  -> m ()
scrollViewSetScrollSnapEpsilon p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollView* ptr)->SetScrollSnapEpsilon($(float v'))} |]

-- | Set whether child elements should be disabled while touch scrolling.
-- void SetAutoDisableChildren(bool disable) { autoDisableChildren_ = disable; };
scrollViewSetAutoDisableChildren :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> Bool -- ^ disable
  -> m ()
scrollViewSetAutoDisableChildren p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void {$(ScrollView* ptr)->SetAutoDisableChildren($(int v') != 0)} |]

-- | Set how much touch movement is needed to trigger child element disabling.
-- void SetAutoDisableThreshold(float amount) { autoDisableThreshold_ = amount; };
scrollViewSetAutoDisableThreshold :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> Float -- ^ amount
  -> m ()
scrollViewSetAutoDisableThreshold p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void {$(ScrollView* ptr)->SetAutoDisableThreshold($(float v'))} |]

-- | Return view offset from the top-left corner.
-- const IntVector2& GetViewPosition() const { return viewPosition_; }
scrollViewGetViewPosition :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m IntVector2
scrollViewGetViewPosition p = liftIO $ do
  let ptr = parentPointer p
  peek =<< [C.exp| const IntVector2* {&$(ScrollView* ptr)->GetViewPosition()} |]

-- | Return content element.
-- UIElement* GetContentElement() const { return contentElement_; }
scrollViewGetContentElement :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m (Ptr UIElement)
scrollViewGetContentElement p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| UIElement* {$(ScrollView* ptr)->GetContentElement()} |]

-- | Return horizontal scroll bar.
-- ScrollBar* GetHorizontalScrollBar() const { return horizontalScrollBar_; }
scrollViewGetHorizontalScrollBar :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m (Ptr ScrollBar)
scrollViewGetHorizontalScrollBar p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| ScrollBar* {$(ScrollView* ptr)->GetHorizontalScrollBar()} |]

-- | Return vertical scroll bar.
-- ScrollBar* GetVerticalScrollBar() const { return verticalScrollBar_; }
scrollViewGetVerticalScrollBar :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m (Ptr ScrollBar)
scrollViewGetVerticalScrollBar p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| ScrollBar* {$(ScrollView* ptr)->GetVerticalScrollBar()} |]

-- | Return scroll panel.
-- BorderImage* GetScrollPanel() const { return scrollPanel_; }
scrollViewGetScrollPanel :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m (Ptr BorderImage)
scrollViewGetScrollPanel p = liftIO $ do
  let ptr = parentPointer p
  [C.exp| BorderImage* {$(ScrollView* ptr)->GetScrollPanel()} |]

-- | Return whether scrollbars are automatically shown/hidden.
-- bool GetScrollBarsAutoVisible() const { return scrollBarsAutoVisible_; }
scrollViewGetScrollBarsAutoVisible :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m Bool
scrollViewGetScrollBarsAutoVisible p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(ScrollView* ptr)->GetScrollBarsAutoVisible()} |]

-- | Return arrow key scroll step.
-- float GetScrollStep() const;
scrollViewGetScrollStep :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m Float
scrollViewGetScrollStep p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollView* ptr)->GetScrollStep()} |]

-- | Return arrow key page step.
-- float GetPageStep() const { return pageStep_; }
scrollViewGetPageStep :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m Float
scrollViewGetPageStep p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollView* ptr)->GetPageStep()} |]

-- | Return scroll deceleration.
-- float GetScrollDeceleration() const { return scrollDeceleration_; }
scrollViewGetScrollDeceleration :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m Float
scrollViewGetScrollDeceleration p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollView* ptr)->GetScrollDeceleration()} |]

-- | Return scroll snap epsilon
-- float GetScrollSnapEpsilon() const { return scrollSnapEpsilon_; }
scrollViewGetScrollSnapEpsilon :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m Float
scrollViewGetScrollSnapEpsilon p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollView* ptr)->GetScrollSnapEpsilon()} |]

-- | Return whether child element will be disabled while touch scrolling.
-- bool GetAutoDisableChildren() const { return autoDisableChildren_; }
scrollViewGetAutoDisableChildren :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m Bool
scrollViewGetAutoDisableChildren p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int {(int)$(ScrollView* ptr)->GetAutoDisableChildren()} |]

-- | Return how much touch movement is needed to trigger child element disabling.
-- float GetAutoDisableThreshold() const { return autoDisableThreshold_; }
scrollViewGetAutoDisableThreshold :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> m Float
scrollViewGetAutoDisableThreshold p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float {$(ScrollView* ptr)->GetAutoDisableThreshold()} |]

-- | Set view position attribute.
-- void SetViewPositionAttr(const IntVector2& value);
scrollViewSetViewPositionAttr :: (Parent ScrollView a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to scrollView or ascentor
  -> IntVector2 -- ^ value
  -> m ()
scrollViewSetViewPositionAttr p v = liftIO $ with v $ \v' -> do
  let ptr = parentPointer p
  [C.exp| void {$(ScrollView* ptr)->SetViewPositionAttr(*$(IntVector2* v'))} |]
