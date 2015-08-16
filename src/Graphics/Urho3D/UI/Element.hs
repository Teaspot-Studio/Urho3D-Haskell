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
  , uiElementSetSize'
  , uiElementSetPosition
  , HorizontalAlignment(..)
  , VerticalAlignment(..)
  , uiElementSetAlignment
  , uiElementSetHorizontalAlignment
  , uiElementSetVerticalAlignment
  , uiElementSetOpacity
  , uiElementSetPriority
  , uiElementAddChild
  , uiElementSetDefaultStyle
  , uiElementSetName
  , uiElementSetMinWidth
  , uiElementSetMinHeight
  , uiElementSetMinSize
  , uiElementSetStyleAuto
  , uiElementSetStyleAutoDefault
  , uiElementSetStyle
  , uiElementSetStyleDefault
  , Corner(..)
  , uiElementSetColor
  , uiElementSetCornerColor
  , LayoutMode(..)
  , uiElementSetLayout
  , uiElementSetLayoutMode
  , uiElementSetLayoutSpacing
  , uiElementSetLayoutBorder
  , uiElementSetLayoutFlexScale
  , uiElementGetSize
  , uiElementGetWidth
  , uiElementGetHeight
  , uiElementGetPosition
  , uiElementGetChild
  , uiElementGetName
  , uiElementGetColor
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Element
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Resource.XMLFile 
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.C.String 
import Data.Proxy 

C.context (C.cppCtx <> sharedUIElementPtrCntx <> uiElementCntx <> stringHashContext <> vector2Context <> rectContext <> contextContext <> xmlFileContext <> stringContext <> colorContext)
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
  deriving (Eq, Ord, Show, Enum)

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
  deriving (Eq, Ord, Show, Enum, Bounded)

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

-- | Return child element by name.
uiElementGetChild :: (Parent UIElement a, Pointer p a, Parent UIElement e, UIElem e, MonadIO m) 
  => p -- ^ Pointer to UI element
  -> String -- ^ Name of element
  -> Bool -- ^ Recursive search?
  -> m (Maybe (Ptr e))
uiElementGetChild p name recursive = liftIO $ withCString name $ \name' -> do 
  let ptr = parentPointer p 
      recursive' = fromBool recursive
  castToChild <$> [C.exp| UIElement* { $(UIElement* ptr)->GetChild(String($(const char* name')), $(int recursive') != 0) } |]

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