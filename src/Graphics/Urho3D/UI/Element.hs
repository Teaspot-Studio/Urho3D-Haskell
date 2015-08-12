{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Element(
    UIElement 
  , uiElementContext
  , SharedUIElement
  , SharedUIElementPtr 
  , uiElementSetVisible
  , uiElementIsVisible
  , UIElem(..)
  , createChild
  , createChildSimple
  , uiElementSetSize
  , uiElementSetPosition
  , HorizontalAlignment(..)
  , VerticalAlignment(..)
  , uiElementSetAlignment
  , uiElementSetOpacity
  , uiElementSetPriority
  , uiElementAddChild
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Element
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.C.String 
import Data.Proxy 

C.context (C.cppCtx <> sharedUIElementPtrCntx <> uiElementCntx <> stringHashContext <> vector2Context <> contextContext)
C.include "<Urho3D/UI/UIElement.h>"
C.using "namespace Urho3D"

uiElementContext :: C.Context 
uiElementContext = sharedUIElementPtrCntx <> uiElementCntx <> stringHashContext

sharedPtr "UIElement" 

instance Createable (Ptr UIElement) where 
  type CreationOptions (Ptr UIElement) = Ptr Context 

  newObject ptr = liftIO $ [C.exp| UIElement* { new UIElement($(Context* ptr)) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(UIElement* ptr) } |]

-- | Returns ui element visiblitity
uiElementIsVisible :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> m Bool 
uiElementIsVisible p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { $(UIElement* ptr)->IsVisible() } |]

-- | Hides or shows UI element
uiElementSetVisible :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Flag of visibility, True - visible, False - hided
  -> m ()
uiElementSetVisible ptr flag = liftIO $ do 
  let ptr' = parentPointer ptr 
      flag' = if flag then 1 else 0
  [C.exp| void { $(UIElement* ptr')->SetVisible($(int flag') != 0) } |]

-- | Create and add a child element and return it.
uiElementCreateChild :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Ptr StringHash -- ^ Element type
  -> String -- ^ Name
  -> Int -- ^ Index
  -> m (Ptr UIElement)
uiElementCreateChild ptr hash name index = liftIO $ withCString name $ \name' -> do 
  let ptr' = parentPointer ptr 
      index' = fromIntegral index
  [C.exp| UIElement* { $(UIElement* ptr')->CreateChild(*$(StringHash* hash), String($(const char* name')), $(int index')) } |]

-- | Elements that can be attached to another gui elements
class UIElem a where 
  -- | Getting hash id of the element kind
  uiElemType :: Proxy a -> Ptr StringHash 

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
createChildSimple ptr = createChild ptr "" (fromIntegral (maxBound :: Word32))

-- | Changes physical size of element
uiElementSetSize :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Width
  -> Int -- ^ Height
  -> m ()
uiElementSetSize ptr width height = liftIO $ do 
  let ptr' = parentPointer ptr 
      width' = fromIntegral width 
      height' = fromIntegral height
  [C.exp| void { $(UIElement* ptr')->SetSize($(int width'), $(int height')) } |]

data HorizontalAlignment =
    AlignmentLeft
  | AlignmentHorizontalCenter
  | AlignmentRight 
  deriving (Eq, Ord, Show, Enum)

data VerticalAlignment = 
    AlignmentTop
  | AlignmentVerticalCenter
  | AlignmentBottom
  deriving (Eq, Ord, Show, Enum)

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

-- | Changes element opacity (inverse of alpha)
uiElementSetOpacity :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Float -- ^ Opacity
  -> m ()
uiElementSetOpacity ptr opacity = liftIO $ do 
  let ptr' = parentPointer ptr 
      opacity' = realToFrac $ opacity
  [C.exp| void { $(UIElement* ptr')->SetOpacity($(float opacity')) } |]

-- | Changes element priority (controls overlaying with other elements)
uiElementSetPriority :: (Parent UIElement a, Pointer p a, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Priority (could be negative)
  -> m ()
uiElementSetPriority ptr p = liftIO $ do 
  let ptr' = parentPointer ptr 
      p' = fromIntegral $ p
  [C.exp| void { $(UIElement* ptr')->SetPriority($(int p')) } |]

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