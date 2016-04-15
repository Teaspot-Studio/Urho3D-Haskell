{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.CustomLogicComponent(
    CustomLogicComponent
  , customLogicComponentContext
  , SharedCustomLogicComponent
  , SharedCustomLogicComponentPtr
  , CustomLogicComponentSetup(..)
  , defaultCustomLogicComponent
  , customComponentTypeInfo
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
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Data.Maybe 
import Data.Monoid
import Foreign 
-- import Foreign.C.String 
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
};
|]

-- | Defines custom components callbacks
data CustomLogicComponentSetup = CustomLogicComponentSetup {
  componentOnSetEnabled :: Ptr Node -> IO () -- ^ Handle enabled/disabled state change. Changes update event subscription.
, componentStart :: Ptr Node -> IO () -- ^ Called when the component is added to a scene node. Other components may not yet exist.
, componentDelayedStart :: Ptr Node -> IO () -- ^ Called before the first update. At this point all other components of the node should exist. Will also be called if update events are not wanted; in that case the event is immediately unsubscribed afterward.
, componentStop :: IO () -- ^ Called when the component is detached from a scene node, usually on destruction. Note that you will no longer have access to the node and scene at that point.
, componentUpdate :: Maybe (Ptr Node -> Float -> IO ()) -- ^ Called on scene update, variable timestep.
, componentPostUpdate :: Maybe (Ptr Node -> Float -> IO ()) -- ^ Called on scene post-update, variable timestep.
, componentFixedUpdate :: Maybe (Ptr Node -> Float -> IO ()) -- ^ Called on physics update, fixed timestep.
, componentFixedPostUpdate :: Maybe (Ptr Node -> Float -> IO ()) -- ^ Called on physics post-update, fixed timestep.
, componentFixedOnNodeSet :: Ptr Node -> IO () -- ^ Handle scene node being assigned at creation.
, componentFixedOnSceneSet :: Ptr Scene -> IO () -- ^ Handle scene being assigned.
}

-- | Helper, all callbacks are Nothing
defaultCustomLogicComponent :: CustomLogicComponentSetup
defaultCustomLogicComponent = CustomLogicComponentSetup {
  componentOnSetEnabled = const $ return ()
, componentStart = const $ return ()
, componentDelayedStart = const $ return ()
, componentStop = return ()
, componentUpdate = Nothing
, componentPostUpdate = Nothing
, componentFixedUpdate = Nothing
, componentFixedPostUpdate = Nothing
, componentFixedOnNodeSet = const $ return ()
, componentFixedOnSceneSet = const $ return ()
}

customLogicComponentContext :: C.Context 
customLogicComponentContext = sharedCustomLogicComponentPtrCntx <> customLogicComponentCntx <> logicComponentContext <> stringHashContext

newCustomLogicComponent :: Ptr Context -> CustomLogicComponentSetup -> IO (Ptr CustomLogicComponent)
newCustomLogicComponent ptr CustomLogicComponentSetup {..} = do 
  component <- [C.exp| CustomLogicComponent* {
    new CustomLogicComponent($(Context* ptr)
      , $funConst:(void (*componentOnSetEnabled)(Node*))
      , $funConst:(void (*componentStart)(Node*))
      , $funConst:(void (*componentDelayedStart)(Node*))
      , $funConst:(void (*componentStop)())
      , $funConst:(void (*updateFunc)(Node*, float))
      , $funConst:(void (*postUpdateFunc)(Node*, float))
      , $funConst:(void (*fixedUpdateFunc)(Node*, float))
      , $funConst:(void (*fixedPostUpdateFunc)(Node*, float))
      , $funConst:(void (*componentFixedOnNodeSet)(Node*))
      , $funConst:(void (*componentFixedOnSceneSet)(Scene*))
      ) 
  } |]
  logicComponentSetUpdateEventMask component emask
  -- print =<< logicComponentGetUpdateEventMask component
  return component
  where 
  prepareFunc :: Maybe (Ptr Node -> Float -> IO ()) -> Ptr Node -> C.CFloat -> IO ()
  prepareFunc = maybe (const . const $ return ()) (\f node t -> f node $ realToFrac t)
  updateFunc = prepareFunc componentUpdate
  postUpdateFunc = prepareFunc componentPostUpdate  
  fixedUpdateFunc = prepareFunc componentFixedUpdate
  fixedPostUpdateFunc = prepareFunc componentFixedPostUpdate
  emask = catMaybes $ [
      const EM'UseUpdate <$> componentUpdate
    , const EM'UsePostUpdate <$> componentPostUpdate
    , const EM'UseFixedUpdate <$> componentFixedUpdate
    , const EM'UseFixedPostUpdate <$> componentFixedPostUpdate
    ]

deleteCustomLogicComponent :: Ptr CustomLogicComponent -> IO ()
deleteCustomLogicComponent ptr = [C.exp| void { delete $(CustomLogicComponent* ptr) } |]

instance Createable (Ptr CustomLogicComponent) where 
  type CreationOptions (Ptr CustomLogicComponent) = (Ptr Context, CustomLogicComponentSetup) 

  newObject = liftIO . uncurry newCustomLogicComponent
  deleteObject = liftIO . deleteCustomLogicComponent

sharedPtr "CustomLogicComponent" 

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''LogicComponent] ''CustomLogicComponent

instance NodeComponent CustomLogicComponent where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = CustomLogicComponent::GetTypeStatic();
    return &h;
  } |]

-- | Return type of custom logic component
customComponentTypeInfo :: Ptr TypeInfo 
customComponentTypeInfo = [C.pure| const TypeInfo* {
    CustomLogicComponent::GetTypeInfoStatic()
  } |]

-- | Create new custom factory for custom component
createCustomComponentFactory :: (Parent Context a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to context
  -> String -- ^ Name of component type
  -> CustomLogicComponentSetup -- ^ Config of custom component 
  -> m (Ptr CustomFactory, Ptr TypeInfo) -- ^ Return new factory and new type info
createCustomComponentFactory p name setup = liftIO $ do 
  let ptr = parentPointer p
      maker :: Ptr Context -> IO (Ptr Object)
      maker cntx = do 
        pobj <- newCustomLogicComponent cntx setup
        return . castToParent $ pobj
  customType <- newTypeInfo name customComponentTypeInfo
  factory <- newCustomFactory ptr customType maker
  return (factory, customType)

-- | Register component in system, after that you can create the component with Node methods
registerCustomComponent :: (Parent Context a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to context
  -> String -- ^ Name of component type
  -> CustomLogicComponentSetup -- ^ Config of custom component 
  -> m (ForeignPtr StringHash) -- ^ Return type hash of component
registerCustomComponent p name setup = liftIO $ do 
  (factory, customType) <- createCustomComponentFactory p name setup
  contextRegisterFactory p factory
  typeInfoGetType customType

-- | Register component in system, after that you can create the component with Node methods
registerCustomComponentCat :: (Parent Context a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to context
  -> String -- ^ Name of component type
  -> String -- ^ Name of category to register fabric with
  -> CustomLogicComponentSetup -- ^ Config of custom component 
  -> m (ForeignPtr StringHash) -- ^ Return type hash of component
registerCustomComponentCat p name cat setup = liftIO $ do 
  (factory, customType) <- createCustomComponentFactory p name setup
  contextRegisterFactoryCat p factory cat
  typeInfoGetType customType