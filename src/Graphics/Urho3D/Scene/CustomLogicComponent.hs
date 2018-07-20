{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.CustomLogicComponent(
    CustomLogicComponent
  , customLogicComponentContext
  , SharedCustomLogicComponent
  , CustomLogicComponentSetup(..)
  , defaultCustomLogicComponent
  , newCustomLogicComponent
  , deleteCustomLogicComponent
  , customComponentTypeInfo
  , getCustomComponentState
  , registerCustomComponent
  , registerCustomComponentCat
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Text.RawString.QQ

import Graphics.Urho3D.Scene.Internal.CustomLogicComponent
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.CustomFactory
import Graphics.Urho3D.Core.TypeInfo
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Container.FlagSet
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Data.IORef
import Data.Maybe
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.LogicComponent
import Graphics.Urho3D.Scene.Node
import Graphics.Urho3D.Scene.Scene
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx <> C.funConstCtx <> customLogicComponentCntx <> logicComponentContext <> sharedCustomLogicComponentPtrCntx <> contextContext <> stringHashContext <> animatableContext <> componentContext <> serializableContext <> sceneContext <> customFactoryContext <> typeInfoContext)
C.include "<Urho3D/Scene/LogicComponent.h>"
C.include "<Urho3D/Core/Context.h>"
C.include "<iostream>"
C.using "namespace Urho3D"

C.verbatim [r|
extern "C" typedef void (*haskellIO)();
extern "C" typedef void (*haskellIOFloat)(Node*, float);
extern "C" typedef void (*haskellIONode)(Node*);
extern "C" typedef void (*haskellIOScene)(Scene*);

class CustomLogicComponent : public LogicComponent {
  URHO3D_OBJECT(CustomLogicComponent, LogicComponent);

  public:

  CustomLogicComponent(Context* context
    , haskellIONode onSetEnabledFunc_
    , haskellIONode startFunc_
    , haskellIONode delayedStartFunc_
    , haskellIO stopFunc_
    , haskellIOFloat updateFunc_
    , haskellIOFloat postUpdateFunc_
    , haskellIOFloat fixedUpdateFunc_
    , haskellIOFloat fixedPostUpdateFunc_
    , haskellIONode onNodeSetFunc_
    , haskellIOScene onSceneSetFunc_
    , void* haskellState_
    ) : LogicComponent(context)
    , onSetEnabledFunc(onSetEnabledFunc_)
    , startFunc(startFunc_)
    , delayedStartFunc(delayedStartFunc_)
    , stopFunc(stopFunc_)
    , updateFunc(updateFunc_)
    , postUpdateFunc(postUpdateFunc_)
    , fixedUpdateFunc(fixedUpdateFunc_)
    , fixedPostUpdateFunc(fixedPostUpdateFunc_)
    , onNodeSetFunc(onNodeSetFunc_)
    , onSceneSetFunc(onSceneSetFunc_)
    , haskellState(haskellState_)
  {

  }

  virtual void OnSetEnabled() {
    LogicComponent::OnSetEnabled();
    if (onSetEnabledFunc) onSetEnabledFunc(node_);
  }

  virtual void Start() {
    LogicComponent::Start();
    if (startFunc) startFunc(node_);
  }

  virtual void DelayedStart() {
    LogicComponent::DelayedStart();
    if (delayedStartFunc) delayedStartFunc(node_);
  }

  virtual void Stop() {
    LogicComponent::Stop();
    if (stopFunc) stopFunc();
  }

  virtual void Update(float timeStep) {
    LogicComponent::Update(timeStep);
    if (updateFunc) updateFunc(node_, timeStep);
  }

  virtual void PostUpdate(float timeStep) {
    LogicComponent::PostUpdate(timeStep);
    if (postUpdateFunc) postUpdateFunc(node_, timeStep);
  }

  virtual void FixedUpdate(float timeStep) {
    LogicComponent::FixedUpdate(timeStep);
    if (fixedUpdateFunc) fixedUpdateFunc(node_, timeStep);
  }

  virtual void FixedPostUpdate(float timeStep) {
    LogicComponent::FixedPostUpdate(timeStep);
    if (fixedPostUpdateFunc) fixedPostUpdateFunc(node_, timeStep);
  }

  virtual void* GetHaskellState() {
    assert(haskellState);
    return haskellState;
  }

  protected:

  virtual void OnNodeSet(Node* node) {
    LogicComponent::OnNodeSet(node);
    if (onNodeSetFunc) onNodeSetFunc(node);
  }

  virtual void OnSceneSet(Scene* scene) {
    LogicComponent::OnSceneSet(scene);
    if (onSceneSetFunc) onSceneSetFunc(scene);
  }

  private:

  haskellIONode onSetEnabledFunc = NULL;
  haskellIONode startFunc = NULL;
  haskellIONode delayedStartFunc = NULL;
  haskellIO stopFunc = NULL;
  haskellIOFloat updateFunc = NULL;
  haskellIOFloat postUpdateFunc = NULL;
  haskellIOFloat fixedUpdateFunc = NULL;
  haskellIOFloat fixedPostUpdateFunc = NULL;
  haskellIONode onNodeSetFunc = NULL;
  haskellIOScene onSceneSetFunc = NULL;
  void* haskellState = NULL;
};
|]

-- | Defines custom components callbacks
data CustomLogicComponentSetup a = CustomLogicComponentSetup {
  -- | Handle enabled/disabled state change. Changes update event subscription.
  componentOnSetEnabled :: IORef a -> Ptr Node -> IO ()
  -- | Called when the component is added to a scene node. Other components may not yet exist.
, componentStart :: IORef a -> Ptr Node -> IO ()
  -- | Called before the first update. At this point all other components of the node should exist. Will also be called if update events are not wanted; in that case the event is immediately unsubscribed afterward.
, componentDelayedStart :: IORef a -> Ptr Node -> IO ()
  -- | Called when the component is detached from a scene node, usually on destruction. Note that you will no longer have access to the node and scene at that point.
, componentStop :: IORef a -> IO ()
  -- | Called on scene update, variable timestep.
, componentUpdate :: Maybe (IORef a -> Ptr Node -> Float -> IO ())
  -- | Called on scene post-update, variable timestep.
, componentPostUpdate :: Maybe (IORef a -> Ptr Node -> Float -> IO ())
  -- | Called on physics update, fixed timestep.
, componentFixedUpdate :: Maybe (IORef a -> Ptr Node -> Float -> IO ())
  -- | Called on physics post-update, fixed timestep.
, componentFixedPostUpdate :: Maybe (IORef a -> Ptr Node -> Float -> IO ())
  -- | Handle scene node being assigned at creation.
, componentFixedOnNodeSet :: IORef a -> Ptr Node -> IO ()
  -- | Handle scene being assigned.
, componentFixedOnSceneSet :: IORef a -> Ptr Scene -> IO ()
}

-- | Helper, all callbacks are Nothing
defaultCustomLogicComponent :: CustomLogicComponentSetup a
defaultCustomLogicComponent = CustomLogicComponentSetup {
  componentOnSetEnabled = const . const $ return ()
, componentStart = const . const $ return ()
, componentDelayedStart = const . const $ return ()
, componentStop = const $ return ()
, componentUpdate = Nothing
, componentPostUpdate = Nothing
, componentFixedUpdate = Nothing
, componentFixedPostUpdate = Nothing
, componentFixedOnNodeSet = const . const $ return ()
, componentFixedOnSceneSet = const . const $ return ()
}

customLogicComponentContext :: C.Context
customLogicComponentContext = sharedCustomLogicComponentPtrCntx <> customLogicComponentCntx <> logicComponentContext <> stringHashContext

newCustomLogicComponent :: forall m a . MonadIO m => Ptr Context -> a -> CustomLogicComponentSetup a -> m (Ptr CustomLogicComponent)
newCustomLogicComponent ptr a CustomLogicComponentSetup {..} = liftIO $ do
  ref <- newIORef a
  let
    componentOnSetEnabledFunc = componentOnSetEnabled ref
    componentStartFunc = componentStart ref
    componentDelayedStartFunc = componentDelayedStart ref
    componentStopFunc = componentStop ref
    componentFixedOnNodeSetFunc = componentFixedOnNodeSet ref
    componentFixedOnSceneSetFunc = componentFixedOnSceneSet ref

    updateFunc = prepareFunc ref componentUpdate
    postUpdateFunc = prepareFunc ref componentPostUpdate
    fixedUpdateFunc = prepareFunc ref componentFixedUpdate
    fixedPostUpdateFunc = prepareFunc ref componentFixedPostUpdate

  statePtr <- castStablePtrToPtr <$> newStablePtr ref

  component <- [C.exp| CustomLogicComponent* {
    new CustomLogicComponent($(Context* ptr)
      , $funConst:(void (*componentOnSetEnabledFunc)(Node*))
      , $funConst:(void (*componentStartFunc)(Node*))
      , $funConst:(void (*componentDelayedStartFunc)(Node*))
      , $funConst:(void (*componentStopFunc)())
      , $funConst:(void (*updateFunc)(Node*, float))
      , $funConst:(void (*postUpdateFunc)(Node*, float))
      , $funConst:(void (*fixedUpdateFunc)(Node*, float))
      , $funConst:(void (*fixedPostUpdateFunc)(Node*, float))
      , $funConst:(void (*componentFixedOnNodeSetFunc)(Node*))
      , $funConst:(void (*componentFixedOnSceneSetFunc)(Scene*))
      , $(void* statePtr)
      )
  } |]
  logicComponentSetUpdateEventMask component emask
  -- print =<< logicComponentGetUpdateEventMask component
  return component
  where

  prepareFunc :: IORef a -> Maybe (IORef a -> Ptr Node -> Float -> IO ()) -> Ptr Node -> C.CFloat -> IO ()
  prepareFunc ref = maybe (const . const $ return ()) (\f node t -> f ref node $ realToFrac t)

  emask = flagsFromList . catMaybes $ [
      const UpdateEventUseUpdate <$> componentUpdate
    , const UpdateEventUsePostUpdate <$> componentPostUpdate
    , const UpdateEventUseFixedUpdate <$> componentFixedUpdate
    , const UpdateEventUseFixedPostUpdate <$> componentFixedPostUpdate
    ]

deleteCustomLogicComponent :: MonadIO m => Ptr CustomLogicComponent -> m ()
deleteCustomLogicComponent ptr = liftIO $ do
  statePtr <- castPtrToStablePtr <$> [C.exp| void* { $(CustomLogicComponent* ptr)->GetHaskellState() } |]
  freeStablePtr statePtr
  [C.exp| void { delete $(CustomLogicComponent* ptr) } |]

sharedPtr "CustomLogicComponent"

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''LogicComponent] ''CustomLogicComponent

instance NodeComponent CustomLogicComponent where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { CustomLogicComponent::GetTypeStatic().Value() } |]

-- | Return type of custom logic component
customComponentTypeInfo :: Ptr TypeInfo
customComponentTypeInfo = [C.pure| const TypeInfo* {
    CustomLogicComponent::GetTypeInfoStatic()
  } |]

-- | Getting component internal state, unsafe as you can cast state to any type
getCustomComponentState :: (Parent CustomLogicComponent a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to component
  -> m (IORef state)
getCustomComponentState p = liftIO $ do
  let ptr = parentPointer p
  statePtr <- castPtrToStablePtr <$> [C.exp| void* { $(CustomLogicComponent* ptr)->GetHaskellState() } |]
  deRefStablePtr statePtr

-- | Create new custom factory for custom component
createCustomComponentFactory :: (Parent Context a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to context
  -> String -- ^ Name of component type
  -> state -- ^ Inital state of component
  -> CustomLogicComponentSetup state -- ^ Config of custom component
  -> m (Ptr CustomFactory, Ptr TypeInfo) -- ^ Return new factory and new type info
createCustomComponentFactory p name a setup = liftIO $ do
  let ptr = parentPointer p
      maker :: Ptr Context -> IO (Ptr Object)
      maker cntx = do
        pobj <- newCustomLogicComponent cntx a setup
        return . castToParent $ pobj
  customType <- newTypeInfo name customComponentTypeInfo
  factory <- newCustomFactory ptr customType maker
  return (factory, customType)

-- | Register component in system, after that you can create the component with Node methods
registerCustomComponent :: (Parent Context a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to context
  -> String -- ^ Name of component type
  -> state -- ^ Inital state of component
  -> CustomLogicComponentSetup state -- ^ Config of custom component
  -> m StringHash -- ^ Return type hash of component
registerCustomComponent p name a setup = liftIO $ do
  (factory, customType) <- createCustomComponentFactory p name a setup
  contextRegisterFactory p factory
  typeInfoGetType customType

-- | Register component in system, after that you can create the component with Node methods
registerCustomComponentCat :: (Parent Context a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to context
  -> String -- ^ Name of component type
  -> String -- ^ Name of category to register fabric with
  -> state -- ^ Inital state of component
  -> CustomLogicComponentSetup state -- ^ Config of custom component
  -> m StringHash -- ^ Return type hash of component
registerCustomComponentCat p name cat a setup = liftIO $ do
  (factory, customType) <- createCustomComponentFactory p name a setup
  contextRegisterFactoryCat p factory cat
  typeInfoGetType customType
