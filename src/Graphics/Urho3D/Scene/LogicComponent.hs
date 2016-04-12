{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.LogicComponent(
    LogicComponent
  , logicComponentContext
  , SharedLogicComponent
  , SharedLogicComponentPtr
  , logicComponentOnSetEnabled
  , logicComponentStart
  , logicComponentDelayedStart
  , logicComponentStop
  , logicComponentUpdate
  , logicComponentPostUpdate
  , logicComponentFixedUpdate
  , logicComponentFixedPostUpdate
  , logicComponentSetUpdateEventMask
  , logicComponentGetUpdateEventMask
  , logicComponentIsDelayedStartCalled
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import GHC.Generics
import Control.DeepSeq 

import Graphics.Urho3D.Scene.Internal.LogicComponent
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

import Graphics.Urho3D.Scene.Animatable 
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx <> logicComponentCntx <> sharedLogicComponentPtrCntx <> contextContext <> stringHashContext <> animatableContext <> componentContext <> serializableContext)
C.include "<Urho3D/Scene/LogicComponent.h>"
C.using "namespace Urho3D" 

logicComponentContext :: C.Context 
logicComponentContext = sharedLogicComponentPtrCntx <> logicComponentCntx <> stringHashContext

newLogicComponent :: Ptr Context -> IO (Ptr LogicComponent)
newLogicComponent ptr = [C.exp| LogicComponent* { new LogicComponent($(Context* ptr)) } |]

deleteLogicComponent :: Ptr LogicComponent -> IO ()
deleteLogicComponent ptr = [C.exp| void { delete $(LogicComponent* ptr) } |]

instance Createable (Ptr LogicComponent) where 
  type CreationOptions (Ptr LogicComponent) = Ptr Context 

  newObject = liftIO . newLogicComponent
  deleteObject = liftIO . deleteLogicComponent

sharedPtr "LogicComponent" 

instance Parent Component LogicComponent where
  castToParent ptr = [C.pure| Component* {(Component*)$(LogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| LogicComponent* {(LogicComponent*)$(Component* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Animatable LogicComponent where
  castToParent ptr = [C.pure| Animatable* {(Animatable*)$(LogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| LogicComponent* {(LogicComponent*)$(Animatable* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Serializable LogicComponent where
  castToParent ptr = [C.pure| Serializable* {(Serializable*)$(LogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| LogicComponent* {(LogicComponent*)$(Serializable* ptr)} |]
    in if child == nullPtr then Nothing else Just child

-- | Data used in 'logicComponentSetUpdateEventMask' for optimizing even handling
data EventMask = 
    EM'UseUpdate -- ^ Bitmask for using the scene update event.
  | EM'UsePostUpdate -- ^ Bitmask for using the scene post-update event.
  | EM'UseFixedUpdate -- ^ Bitmask for using the physics update event.
  | EM'UseFixedPostUpdate -- ^ Bitmask for using the physics post-update event.
  deriving (Generic, Show, Eq, Ord)

instance NFData EventMask

instance Enum EventMask where 
  fromEnum e = case e of 
    EM'UseUpdate -> 0x01
    EM'UsePostUpdate -> 0x02
    EM'UseFixedUpdate -> 0x04 
    EM'UseFixedPostUpdate -> 0x08
  toEnum i = case i of
    0x01 -> EM'UseUpdate
    0x02 -> EM'UsePostUpdate
    0x04 -> EM'UseFixedUpdate
    0x08 -> EM'UseFixedPostUpdate
    _ -> EM'UseFixedPostUpdate

-- | Handle enabled/disabled state change. Changes update event subscription.
logicComponentOnSetEnabled :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> m ()
logicComponentOnSetEnabled p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(LogicComponent* ptr)->OnSetEnabled() } |]

-- | Called when the component is added to a scene node. Other components may not yet exist.
logicComponentStart :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> m ()
logicComponentStart p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(LogicComponent* ptr)->Start() } |]

-- | Called before the first update. At this point all other components of the node should exist. Will also be called if update events are not wanted; in that case the event is immediately unsubscribed afterward.
logicComponentDelayedStart :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> m ()
logicComponentDelayedStart p = liftIO $ do 
  let ptr = parentPointer p
  [C.exp| void { $(LogicComponent* ptr)->DelayedStart() } |]

-- | Called when the component is detached from a scene node, usually on destruction. Note that you will no longer have access to the node and scene at that point.
logicComponentStop :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> m ()
logicComponentStop p = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(LogicComponent* ptr)->Stop() } |]

-- | Called on scene update, variable timestep.
logicComponentUpdate :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> Float -- ^ Time step
  -> m ()
logicComponentUpdate p t = liftIO $ do 
  let ptr = parentPointer p 
      t' = realToFrac t
  [C.exp| void { $(LogicComponent* ptr)->Update($(float t')) } |]

-- | Called on scene post-update, variable timestep.
logicComponentPostUpdate :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> Float -- ^ Time step
  -> m ()
logicComponentPostUpdate p t = liftIO $ do 
  let ptr = parentPointer p 
      t' = realToFrac t
  [C.exp| void { $(LogicComponent* ptr)->PostUpdate($(float t')) } |]

-- | Called on physics update, fixed timestep.
logicComponentFixedUpdate :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> Float -- ^ Time step
  -> m ()
logicComponentFixedUpdate p t = liftIO $ do 
  let ptr = parentPointer p 
      t' = realToFrac t
  [C.exp| void { $(LogicComponent* ptr)->FixedUpdate($(float t')) } |]

-- | Called on physics post-update, fixed timestep.
logicComponentFixedPostUpdate :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> Float -- ^ Time step
  -> m ()
logicComponentFixedPostUpdate p t = liftIO $ do 
  let ptr = parentPointer p 
      t' = realToFrac t
  [C.exp| void { $(LogicComponent* ptr)->FixedPostUpdate($(float t')) } |]

-- | Set what update events should be subscribed to. Use this for optimization: by default all are in use. Note that this is not an attribute and is not saved or network-serialized, therefore it should always be called eg. in the subclass constructor.
logicComponentSetUpdateEventMask :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> [EventMask] -- ^ Flags of events that are enabled
  -> m ()
logicComponentSetUpdateEventMask p es = liftIO $ do 
  let ptr = parentPointer p 
      w = fromIntegral $ toByteBitset es
  [C.exp| void { $(LogicComponent* ptr)->SetUpdateEventMask($(unsigned char w)) } |]

-- | Return what update events are subscribed to.
logicComponentGetUpdateEventMask :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> m [EventMask]
logicComponentGetUpdateEventMask p = liftIO $ do 
  let ptr = parentPointer p 
  fromByteBitset . fromIntegral <$> [C.exp| unsigned char { $(LogicComponent* ptr)->GetUpdateEventMask() } |]

-- | Return whether the DelayedStart() function has been called.
logicComponentIsDelayedStartCalled :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> m Bool
logicComponentIsDelayedStartCalled p = liftIO $ do 
  let ptr = parentPointer p 
  toBool <$> [C.exp| int { $(LogicComponent* ptr)->IsDelayedStartCalled() } |]
