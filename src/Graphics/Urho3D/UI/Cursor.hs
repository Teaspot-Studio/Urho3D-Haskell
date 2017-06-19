{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.Cursor(
    Cursor
  , SharedCursor
  , WeakCursor
  , CursorShape(..)
  , CursorShapeInfo(..)
  , HasImage(..)
  , HasTexture(..)
  , HasImageRect(..)
  , HasHotSpot(..)
  , HasOsCursor(..)
  , HasSystemDefined(..)
  , HasSystemCursor(..)
  , cursorContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.UI.Internal.Cursor
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Resource.Image
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx
  <> cursorCntx
  <> contextContext
  <> borderImageContext
  <> uiElementContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> sharedCursorPtrCntx
  <> weakCursorPtrCntx
  <> imageContext
  <> textureContext
  <> rectContext
  <> vector2Context
  )

C.include "<Urho3D/UI/Cursor.h>"
C.using "namespace Urho3D"

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

cursorContext :: C.Context
cursorContext = cursorCntx
  <> borderImageContext
  <> sharedCursorPtrCntx
  <> uiElementContext
  <> weakCursorPtrCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement, ''BorderImage] ''Cursor

sharedPtr "Cursor"
sharedWeakPtr "Cursor"

C.verbatim "typedef SharedPtr<Image> SharedImage;"
C.verbatim "typedef SharedPtr<Texture> SharedTexture;"

newCursor :: Ptr Context -> IO (Ptr Cursor)
newCursor ptr = [C.exp| Cursor* { new Cursor( $(Context* ptr) ) } |]

deleteCursor :: Ptr Cursor -> IO ()
deleteCursor ptr = [C.exp| void { delete $(Cursor* ptr) } |]

instance Creatable (Ptr Cursor) where
  type CreationOptions (Ptr Cursor) = Ptr Context

  newObject = liftIO . newCursor
  deleteObject = liftIO . deleteCursor

instance UIElem Cursor where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Cursor::GetTypeStatic().Value() } |]

instance Storable CursorShapeInfo where
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(CursorShapeInfo) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<CursorShapeInfo>::AlignmentOf } |]
  peek ptr = do
    _cursorShapeInfoImage <- peekSharedPtr =<< [C.exp| SharedImage* { new SharedPtr<Image>($(CursorShapeInfo* ptr)->image_) } |]
    _cursorShapeInfoTexture <- peekSharedPtr =<< [C.exp| SharedTexture* { new SharedPtr<Texture>($(CursorShapeInfo* ptr)->texture_) } |]
    _cursorShapeInfoImageRect <- peek =<< [C.exp| IntRect* { &$(CursorShapeInfo* ptr)->imageRect_ } |]
    _cursorShapeInfoHotSpot <- peek =<< [C.exp| IntVector2* { &$(CursorShapeInfo* ptr)->hotSpot_ } |]
    _cursorShapeInfoOsCursor <- [C.exp| void* { (void*)$(CursorShapeInfo* ptr)->osCursor_ } |]
    _cursorShapeInfoSystemDefined <- toBool <$> [C.exp| int { (int)$(CursorShapeInfo* ptr)->systemDefined_ } |]
    _cursorShapeInfoSystemCursor <- fromIntegral <$> [C.exp| int { $(CursorShapeInfo* ptr)->systemCursor_ } |]
    return CursorShapeInfo {..}
  poke ptr CursorShapeInfo {..} =
    withSharedPtr _cursorShapeInfoImage $ \_cursorShapeInfoImage' ->
    withSharedPtr _cursorShapeInfoTexture $ \_cursorShapeInfoTexture' ->
    with _cursorShapeInfoImageRect $ \_cursorShapeInfoImageRect' ->
    with _cursorShapeInfoHotSpot $ \_cursorShapeInfoHotSpot' ->
    [C.block| void {
      $(CursorShapeInfo* ptr)->image_ = SharedPtr<Image>(*$(SharedImage* _cursorShapeInfoImage'));
      $(CursorShapeInfo* ptr)->texture_ = SharedPtr<Texture>(*$(SharedTexture* _cursorShapeInfoTexture'));
      $(CursorShapeInfo* ptr)->imageRect_ = *$(IntRect* _cursorShapeInfoImageRect');
      $(CursorShapeInfo* ptr)->hotSpot_ = *$(IntVector2* _cursorShapeInfoHotSpot');
      $(CursorShapeInfo* ptr)->osCursor_ = (SDL_Cursor*)$(void* _cursorShapeInfoOsCursor);
      $(CursorShapeInfo* ptr)->systemDefined_ = $(int _cursorShapeInfoSystemDefined') != 0;
      $(CursorShapeInfo* ptr)->systemCursor_ = $(int _cursorShapeInfoSystemCursor');
      } |]
    where
      _cursorShapeInfoSystemDefined' = fromBool _cursorShapeInfoSystemDefined
      _cursorShapeInfoSystemCursor' = fromIntegral _cursorShapeInfoSystemCursor
