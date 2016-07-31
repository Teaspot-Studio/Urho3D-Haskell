{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.BillboardSet(
    BillboardSet
  , Billboard(..)
  , HasPosition(..)
  , HasUv(..)
  , HasColor(..)
  , HasRotation(..)
  , HasDirection(..)
  , HasEnabled(..)
  , HasSortDistance(..)
  , HasScreenScaleFactor(..)
  , billboardSetContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.BillboardSet
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Graphics.Drawable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Scene.Node 
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.Color 
import Graphics.Urho3D.Math.Rect
import Graphics.Urho3D.Math.Vector2
import Graphics.Urho3D.Math.Vector3
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx 
  <> contextContext
  <> billboardSetCntx 
  <> drawableContext
  <> componentContext 
  <> animatableContext 
  <> serializableContext 
  <> objectContext
  <> vector3Context
  <> vector2Context
  <> rectContext
  <> colorContext)

C.include "<Urho3D/Graphics/BillboardSet.h>"
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


billboardSetContext :: C.Context 
billboardSetContext = componentContext <> billboardSetCntx

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Drawable] ''BillboardSet

instance Creatable (Ptr BillboardSet) where 
  type CreationOptions (Ptr BillboardSet) = Ptr Context

  newObject ptr = liftIO $ [C.exp| BillboardSet* { new BillboardSet($(Context* ptr)) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(BillboardSet* ptr) } |]

instance NodeComponent BillboardSet where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = BillboardSet::GetTypeStatic();
    return &h;
  } |]

instance Storable Billboard where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Billboard) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Billboard>::AlignmentOf } |]
  peek ptr = do 
    _billboardPosition <- peek =<< [C.exp| Vector3* {&$(Billboard* ptr)->position_} |]
    _billboardSize <- peek =<< [C.exp| Vector2* {&$(Billboard* ptr)->size_} |]
    _billboardUv <- peek =<< [C.exp| Rect* {&$(Billboard* ptr)->uv_} |]
    _billboardColor <- peek =<< [C.exp| Color* {&$(Billboard* ptr)->color_} |]
    _billboardRotation <- realToFrac <$> [C.exp| float {$(Billboard* ptr)->rotation_} |]
    _billboardDirection <- peek =<< [C.exp| Vector3* {&$(Billboard* ptr)->direction_} |]
    _billboardEnabled <- toBool <$> [C.exp| int {$(Billboard* ptr)->enabled_} |]
    _billboardSortDistance <- realToFrac <$> [C.exp| float {$(Billboard* ptr)->sortDistance_} |]
    _billboardScreenScaleFactor <- realToFrac <$> [C.exp| float {$(Billboard* ptr)->screenScaleFactor_} |]
    return Billboard {..}
  poke ptr (Billboard {..}) = 
    with _billboardPosition $ \_billboardPosition' ->
    with _billboardSize $ \_billboardSize' ->
    with _billboardUv $ \_billboardUv' ->
    with _billboardColor $ \_billboardColor' ->
    with _billboardDirection $ \_billboardDirection' ->
    [C.block| void { 
      $(Billboard* ptr)->position_ = *$(Vector3* _billboardPosition');
      $(Billboard* ptr)->size_ = *$(Vector2* _billboardSize');
      $(Billboard* ptr)->uv_ = *$(Rect* _billboardUv');
      $(Billboard* ptr)->color_ = *$(Color* _billboardColor');
      $(Billboard* ptr)->rotation_ = $(float _billboardRotation');
      $(Billboard* ptr)->direction_ = *$(Vector3* _billboardDirection');
      $(Billboard* ptr)->enabled_ = $(int _billboardEnabled') != 0;
      $(Billboard* ptr)->sortDistance_ = $(float _billboardSortDistance');
      $(Billboard* ptr)->screenScaleFactor_ = $(float _billboardScreenScaleFactor');
    } |]
    where
    _billboardRotation' = realToFrac _billboardRotation
    _billboardEnabled' = fromBool _billboardEnabled
    _billboardSortDistance' = realToFrac _billboardSortDistance
    _billboardScreenScaleFactor' = realToFrac _billboardScreenScaleFactor