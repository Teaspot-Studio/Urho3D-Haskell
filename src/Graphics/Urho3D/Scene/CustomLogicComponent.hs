{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Scene.CustomLogicComponent(
    CustomLogicComponent
  , customLogicComponentContext
  , SharedCustomLogicComponent
  , SharedCustomLogicComponentPtr
  , CustomLogicComponentSetup(..)
  , defaultCustomLogicComponent
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C
import Text.RawString.QQ

import Graphics.Urho3D.Scene.Internal.CustomLogicComponent
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
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

C.context (C.cppCtx <> C.funConstCtx <> customLogicComponentCntx <> logicComponentContext <> sharedCustomLogicComponentPtrCntx <> contextContext <> stringHashContext <> animatableContext <> componentContext <> serializableContext <> sceneContext)
C.include "<Urho3D/Scene/LogicComponent.h>"
C.using "namespace Urho3D" 

C.verbatim [r|
extern "C" typedef void (*haskellIO)();
extern "C" typedef void (*haskellIOFloat)(float);
extern "C" typedef void (*haskellIONode)(Node*);
extern "C" typedef void (*haskellIOScene)(Scene*);

class CustomLogicComponent : public LogicComponent {
  URHO3D_OBJECT(CustomLogicComponent, LogicComponent);
  
  public:

  CustomLogicComponent(Context* context
    , haskellIO onSetEnabledFunc_
    , haskellIO startFunc_
    , haskellIO delayedStartFunc_
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
    unsigned char mask = 0;
    if (updateFunc) mask |= USE_UPDATE;
    if (postUpdateFunc) mask |= USE_POSTUPDATE;
    if (fixedUpdateFunc) mask |= USE_FIXEDUPDATE;
    if (fixedPostUpdateFunc) mask |= USE_FIXEDPOSTUPDATE;    
    SetUpdateEventMask(mask);
  }

  virtual void OnSetEnabled() {
    if (onSetEnabledFunc) onSetEnabledFunc();
  }

  virtual void Start() { 
    if (startFunc) startFunc();
  }

  virtual void DelayedStart() { 
    if (delayedStartFunc) delayedStartFunc();
  }

  virtual void Stop() { 
    if (stopFunc) stopFunc();
  }

  virtual void Update(float timeStep) {
    if (updateFunc) updateFunc(timeStep);
  }

  virtual void PostUpdate(float timeStep) {
    if (postUpdateFunc) postUpdateFunc(timeStep);
  }

  virtual void FixedUpdate(float timeStep) {
    if (fixedUpdateFunc) fixedUpdateFunc(timeStep);
  }

  virtual void FixedPostUpdate(float timeStep) {
    if (fixedPostUpdateFunc) fixedPostUpdateFunc(timeStep);
  }

  protected:

  virtual void OnNodeSet(Node* node) {
    if (onNodeSetFunc) onNodeSetFunc(node);
  }

  virtual void OnSceneSet(Scene* scene) {
    if (onSceneSetFunc) onSceneSetFunc(scene);
  }

  private:

  haskellIO onSetEnabledFunc = NULL;
  haskellIO startFunc = NULL;
  haskellIO delayedStartFunc = NULL;
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
  componentOnSetEnabled :: IO () -- ^ Handle enabled/disabled state change. Changes update event subscription.
, componentStart :: IO () -- ^ Called when the component is added to a scene node. Other components may not yet exist.
, componentDelayedStart :: IO () -- ^ Called before the first update. At this point all other components of the node should exist. Will also be called if update events are not wanted; in that case the event is immediately unsubscribed afterward.
, componentStop :: IO () -- ^ Called when the component is detached from a scene node, usually on destruction. Note that you will no longer have access to the node and scene at that point.
, componentUpdate :: Maybe (Float -> IO ()) -- ^ Called on scene update, variable timestep.
, componentPostUpdate :: Maybe (Float -> IO ()) -- ^ Called on scene post-update, variable timestep.
, componentFixedUpdate :: Maybe (Float -> IO ()) -- ^ Called on physics update, fixed timestep.
, componentFixedPostUpdate :: Maybe (Float -> IO ()) -- ^ Called on physics post-update, fixed timestep.
, componentFixedOnNodeSet :: Ptr Node -> IO () -- ^ Handle scene node being assigned at creation.
, componentFixedOnSceneSet :: Ptr Scene -> IO () -- ^ Handle scene being assigned.
}

-- | Helper, all callbacks are Nothing
defaultCustomLogicComponent :: CustomLogicComponentSetup
defaultCustomLogicComponent = CustomLogicComponentSetup {
  componentOnSetEnabled = return ()
, componentStart = return ()
, componentDelayedStart = return ()
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
      , $funConst:(void (*componentOnSetEnabled)())
      , $funConst:(void (*componentStart)())
      , $funConst:(void (*componentDelayedStart)())
      , $funConst:(void (*componentStop)())
      , $funConst:(void (*updateFunc)(float))
      , $funConst:(void (*postUpdateFunc)(float))
      , $funConst:(void (*fixedUpdateFunc)(float))
      , $funConst:(void (*fixedPostUpdateFunc)(float))
      , $funConst:(void (*componentFixedOnNodeSet)(Node*))
      , $funConst:(void (*componentFixedOnSceneSet)(Scene*))
      ) 
  } |]
  logicComponentSetUpdateEventMask component emask
  return component
  where 
  prepareFunc = maybe (const $ return ()) (. realToFrac)
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

instance Parent LogicComponent CustomLogicComponent where
  castToParent ptr = [C.pure| LogicComponent* {(LogicComponent*)$(CustomLogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| CustomLogicComponent* {(CustomLogicComponent*)$(LogicComponent* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Component CustomLogicComponent where
  castToParent ptr = [C.pure| Component* {(Component*)$(CustomLogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| CustomLogicComponent* {(CustomLogicComponent*)$(Component* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Animatable CustomLogicComponent where
  castToParent ptr = [C.pure| Animatable* {(Animatable*)$(CustomLogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| CustomLogicComponent* {(CustomLogicComponent*)$(Animatable* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance Parent Serializable CustomLogicComponent where
  castToParent ptr = [C.pure| Serializable* {(Serializable*)$(CustomLogicComponent* ptr)} |]
  castToChild ptr = let
    child = [C.pure| CustomLogicComponent* {(CustomLogicComponent*)$(Serializable* ptr)} |]
    in if child == nullPtr then Nothing else Just child

instance NodeComponent CustomLogicComponent where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = CustomLogicComponent::GetTypeStatic();
    return &h;
  } |]

