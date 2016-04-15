{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Zone(
    Zone
  , zoneContext
  , zoneSetBoundingBox
  , zoneSetAmbientColor
  , zoneSetFogColor
  , zoneSetFogStart
  , zoneSetFogEnd
  , zoneSetFogHeight
  , zoneSetFogHeightScale
  , zoneSetPriority
  , zoneSetHeightFog
  , zoneSetOverride
  , zoneSetAmbientGradient
  , zoneSetZoneTexture
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Zone
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Texture
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Scene.Node 
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Math.Color
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> zoneCntx <> componentContext <> animatableContext <> serializableContext <> objectContext <> boundingBoxContext <> colorContext <> textureContext)
C.include "<Urho3D/Graphics/Zone.h>"
C.using "namespace Urho3D"

zoneContext :: C.Context 
zoneContext = componentContext <> zoneCntx

instance Parent Component Zone where
  castToParent ptr = [C.pure| Component* {(Component*)$(Zone* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Zone* {(Zone*)$(Component* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Animatable Zone where
  castToParent ptr = [C.pure| Animatable* {(Animatable*)$(Zone* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Zone* {(Zone*)$(Animatable* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Serializable Zone where
  castToParent ptr = [C.pure| Serializable* {(Serializable*)$(Zone* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Zone* {(Zone*)$(Serializable* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Object Zone where
  castToParent ptr = [C.pure| Object* {(Object*)$(Zone* ptr)} |]
  castToChild ptr = let
    child = [C.pure| Zone* {(Zone*)$(Object* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance NodeComponent Zone where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = Zone::GetTypeStatic();
    return &h;
  } |]

-- | Set local-space bounding box. Will be used as an oriented bounding box to test whether objects or the camera are inside.
zoneSetBoundingBox :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> BoundingBox -- ^ Bounding box of zone
  -> m ()
zoneSetBoundingBox p bb = liftIO $ with bb $ \bb' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Zone* ptr)->SetBoundingBox(*$(BoundingBox* bb'))} |]

-- | Set ambient color
zoneSetAmbientColor :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Color -- ^ Color of ambient
  -> m ()
zoneSetAmbientColor p c = liftIO $ with c $ \c' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Zone* ptr)->SetAmbientColor(*$(Color* c'))} |]

-- | Set fog color.
zoneSetFogColor :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Color -- ^ Color of fog
  -> m ()
zoneSetFogColor p c = liftIO $ with c $ \c' -> do 
  let ptr = parentPointer p 
  [C.exp| void {$(Zone* ptr)->SetFogColor(*$(Color* c'))} |]

-- | Set fog start distance.
zoneSetFogStart :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Float -- ^ Start distance
  -> m ()
zoneSetFogStart p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = realToFrac v
  [C.exp| void {$(Zone* ptr)->SetFogStart($(float v'))} |]

-- | Set fog end distance.
zoneSetFogEnd :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Float -- ^ End distance
  -> m ()
zoneSetFogEnd p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = realToFrac v
  [C.exp| void {$(Zone* ptr)->SetFogEnd($(float v'))} |]

-- | Set fog height distance relative to the scene node's world position. Effective only in height fog mode.
zoneSetFogHeight :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Float -- ^ height
  -> m ()
zoneSetFogHeight p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = realToFrac v
  [C.exp| void {$(Zone* ptr)->SetFogHeight($(float v'))} |]

-- | Set fog height scale. Effective only in height fog mode.
zoneSetFogHeightScale :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Float -- ^ scale
  -> m ()
zoneSetFogHeightScale p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = realToFrac v
  [C.exp| void {$(Zone* ptr)->SetFogHeightScale($(float v'))} |]

-- | Set zone priority. If an object or camera is inside several zones, the one with highest priority is used.
zoneSetPriority :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Int -- ^ priority
  -> m ()
zoneSetPriority p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = fromIntegral v
  [C.exp| void {$(Zone* ptr)->SetPriority($(int v'))} |]

-- | Set height fog mode.
zoneSetHeightFog :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Bool -- ^ Enable flag
  -> m ()
zoneSetHeightFog p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = fromBool v
  [C.exp| void {$(Zone* ptr)->SetHeightFog((bool)$(int v'))} |]

-- | Set override mode. If camera is inside an override zone, that zone will be used for all rendered objects instead of their own zone.
zoneSetOverride :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Bool -- ^ Override flag
  -> m ()
zoneSetOverride p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = fromBool v
  [C.exp| void {$(Zone* ptr)->SetOverride((bool)$(int v'))} |]

-- | Set ambient gradient mode. In gradient mode ambient color is interpolated from neighbor zones.
zoneSetAmbientGradient :: (Parent Zone a, Pointer p a, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> Bool -- ^ Gradient flag
  -> m ()
zoneSetAmbientGradient p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = fromBool v
  [C.exp| void {$(Zone* ptr)->SetAmbientGradient((bool)$(int v'))} |]

-- | Set zone texture. This will be bound to the zone texture unit when rendering objects inside the zone. Note that the default shaders do not use it.
zoneSetZoneTexture :: (Parent Zone a, Pointer p a, Parent Texture b, Pointer ptex b, MonadIO m) 
  => p -- ^ Zone pointer or child
  -> ptex -- ^ Pointer to texture
  -> m ()
zoneSetZoneTexture p ptex = liftIO $ do 
  let ptr = parentPointer p 
      ptex' = parentPointer ptex 
  [C.exp| void {$(Zone* ptr)->SetZoneTexture($(Texture* ptex'))} |]