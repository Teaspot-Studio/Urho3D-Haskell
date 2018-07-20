{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.LogicComponent(
    LogicComponent
  , logicComponentContext
  , SharedLogicComponent
  , UpdateEvent(..)
  , UpdateEventFlags
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
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.FlagSet
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> logicComponentCntx <> sharedLogicComponentPtrCntx <> contextContext <> stringHashContext <> animatableContext <> componentContext <> serializableContext <> objectContext)
C.include "<Urho3D/Scene/LogicComponent.h>"
C.using "namespace Urho3D"

logicComponentContext :: C.Context
logicComponentContext = sharedLogicComponentPtrCntx <> logicComponentCntx <> stringHashContext

newLogicComponent :: Ptr Context -> IO (Ptr LogicComponent)
newLogicComponent ptr = [C.exp| LogicComponent* { new LogicComponent($(Context* ptr)) } |]

deleteLogicComponent :: Ptr LogicComponent -> IO ()
deleteLogicComponent ptr = [C.exp| void { delete $(LogicComponent* ptr) } |]

instance Creatable (Ptr LogicComponent) where
  type CreationOptions (Ptr LogicComponent) = Ptr Context

  newObject = liftIO . newLogicComponent
  deleteObject = liftIO . deleteLogicComponent

sharedPtr "LogicComponent"

deriveParents [''Object, ''Serializable, ''Animatable, ''Component] ''LogicComponent

instance NodeComponent LogicComponent where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { LogicComponent::GetTypeStatic().Value() } |]

-- | Data used in 'logicComponentSetUpdateEventMask' for optimizing even handling
data UpdateEvent =
    UpdateEventUseUpdate -- ^ Bitmask for using the scene update event.
  | UpdateEventUsePostUpdate -- ^ Bitmask for using the scene post-update event.
  | UpdateEventUseFixedUpdate -- ^ Bitmask for using the physics update event.
  | UpdateEventUseFixedPostUpdate -- ^ Bitmask for using the physics post-update event.
  deriving (Generic, Show, Eq, Ord)

instance NFData UpdateEvent

instance Enum UpdateEvent where
  fromEnum e = case e of
    UpdateEventUseUpdate -> 0x01
    UpdateEventUsePostUpdate -> 0x02
    UpdateEventUseFixedUpdate -> 0x04
    UpdateEventUseFixedPostUpdate -> 0x08
  toEnum i = case i of
    0x01 -> UpdateEventUseUpdate
    0x02 -> UpdateEventUsePostUpdate
    0x04 -> UpdateEventUseFixedUpdate
    0x08 -> UpdateEventUseFixedPostUpdate
    _ -> UpdateEventUseFixedPostUpdate

type UpdateEventFlags = FlagSet Word32 UpdateEvent

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
  -> UpdateEventFlags -- ^ Flags of events that are enabled
  -> m ()
logicComponentSetUpdateEventMask p es = liftIO $ do
  let ptr = parentPointer p
      w = fromIntegral $ unFlagSet es
  [C.exp| void { $(LogicComponent* ptr)->SetUpdateEventMask((UpdateEventFlags)$(unsigned int w)) } |]

-- | Return what update events are subscribed to.
logicComponentGetUpdateEventMask :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> m UpdateEventFlags
logicComponentGetUpdateEventMask p = liftIO $ do
  let ptr = parentPointer p
  FlagSet . fromIntegral <$> [C.exp| unsigned int { (unsigned)$(LogicComponent* ptr)->GetUpdateEventMask() } |]

-- | Return whether the DelayedStart() function has been called.
logicComponentIsDelayedStartCalled :: (Parent LogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to logic component
  -> m Bool
logicComponentIsDelayedStartCalled p = liftIO $ do
  let ptr = parentPointer p
  toBool <$> [C.exp| int { $(LogicComponent* ptr)->IsDelayedStartCalled() } |]
