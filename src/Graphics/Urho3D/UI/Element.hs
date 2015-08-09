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
  , UIElem(..)
  , createChild
  , createChildSimple
  , uiElementSetSize
  , HorizontalAlignment(..)
  , VerticalAlignment(..)
  , uiElementSetAlignment
  , uiElementSetOpacity
  , uiElementSetPriority
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Element
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.C.String 
import Data.Proxy 

C.context (C.cppCtx <> sharedUIElementPtrCntx <> uiElementCntx <> stringHashContext)
C.include "<Urho3D/UI/UIElement.h>"
C.using "namespace Urho3D"

uiElementContext :: C.Context 
uiElementContext = sharedUIElementPtrCntx <> uiElementCntx <> stringHashContext

sharedPtr "UIElement" 

-- | Hides or shows UI element
uiElementSetVisible :: (Pointer p UIElement, MonadIO m) => p -- ^ Pointer to UI element
  -> Bool -- ^ Flag of visibility, True - visible, False - hided
  -> m ()
uiElementSetVisible ptr flag = liftIO $ do 
  let ptr' = pointer ptr 
      flag' = if flag then 1 else 0
  [C.exp| void { $(UIElement* ptr')->SetVisible($(int flag') != 0) } |]

-- | Create and add a child element and return it.
uiElementCreateChild :: (Pointer p UIElement, MonadIO m) => p -- ^ Pointer to UI element
  -> Ptr StringHash -- ^ Element type
  -> String -- ^ Name
  -> Int -- ^ Index
  -> m (Ptr UIElement)
uiElementCreateChild ptr hash name index = liftIO $ withCString name $ \name' -> do 
  let ptr' = pointer ptr 
      index' = fromIntegral index
  [C.exp| UIElement* { $(UIElement* ptr')->CreateChild(*$(StringHash* hash), String($(const char* name')), $(int index')) } |]

-- | Elements that can be attached to another gui elements
class UIElem a where 
  -- | Getting hash id of the element kind
  uiElemType :: Proxy a -> Ptr StringHash 

createChild :: forall p m e . (Pointer p UIElement, MonadIO m, UIElem e) 
  => p -- ^ Pointer to UI element
  -> String -- ^ Name
  -> Int -- ^ Index
  -> m (Ptr e)
createChild ptr name index = do 
  eptr <- uiElementCreateChild ptr (uiElemType (Proxy :: Proxy e)) name index 
  return $ castPtr eptr

createChildSimple :: (Pointer p UIElement, MonadIO m, UIElem e) => p -- ^ Pointer to UI element
  -> m (Ptr e) 
createChildSimple ptr = createChild ptr "" (fromIntegral (maxBound :: Word32))

-- | Changes physical size of element
uiElementSetSize :: (Pointer p UIElement, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Width
  -> Int -- ^ Height
  -> m ()
uiElementSetSize ptr width height = liftIO $ do 
  let ptr' = pointer ptr 
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
uiElementSetAlignment :: (Pointer p UIElement, MonadIO m) => p -- ^ Pointer to UI element
  -> HorizontalAlignment -- ^ Horizontal behavior
  -> VerticalAlignment -- ^ Vertical behaivor
  -> m ()
uiElementSetAlignment ptr ha va = liftIO $ do 
  let ptr' = pointer ptr 
      ha' = fromIntegral $ fromEnum ha 
      va' = fromIntegral $ fromEnum va
  [C.exp| void { $(UIElement* ptr')->SetAlignment((HorizontalAlignment)$(int ha'), (VerticalAlignment)$(int va')) } |]

-- | Changes element opacity (inverse of alpha)
uiElementSetOpacity :: (Pointer p UIElement, MonadIO m) => p -- ^ Pointer to UI element
  -> Float -- ^ Opacity
  -> m ()
uiElementSetOpacity ptr opacity = liftIO $ do 
  let ptr' = pointer ptr 
      opacity' = realToFrac $ opacity
  [C.exp| void { $(UIElement* ptr')->SetOpacity($(float opacity')) } |]

-- | Changes element priority (controls overlaying with other elements)
uiElementSetPriority :: (Pointer p UIElement, MonadIO m) => p -- ^ Pointer to UI element
  -> Int -- ^ Priority (could be negative)
  -> m ()
uiElementSetPriority ptr p = liftIO $ do 
  let ptr' = pointer ptr 
      p' = fromIntegral $ p
  [C.exp| void { $(UIElement* ptr')->SetPriority($(int p')) } |]