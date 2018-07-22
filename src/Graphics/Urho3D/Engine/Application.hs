{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Engine.Application(
    Application
  , ApplicationCreate(..)
  , SharedApplication
  , applicationContext
  , startupParameter
  , applicationEngine
  , applicationRun
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Engine.Internal.Application
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Engine.Engine
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Data.Monoid
import Data.StateVar
import Text.RawString.QQ
import Foreign
import Foreign.C.String

import Graphics.Urho3D.Multithread

C.context (C.cppCtx <> C.funConstCtx <> applicationCntx <> sharedApplicationHPtrCntx <> contextContext <> variantContext <> objectContext <> engineContext)
C.include "<Urho3D/Engine/Engine.h>"
C.include "<Urho3D/Engine/Application.h>"
C.include "<Urho3D/Core/CoreEvents.h>"
C.include "<iostream>"
C.using "namespace Urho3D"

type SharedApplication = SharedApplicationH

C.verbatim [r|
class ApplicationH;

extern "C" typedef void (*haskellIOFunc)();
extern "C" typedef void (*haskellAppIOFunc)(ApplicationH*);

class ApplicationH : public Application {

  URHO3D_OBJECT(ApplicationH, Application);

  public:

  ApplicationH(Context* context
    , haskellAppIOFunc setupFunc_
    , haskellAppIOFunc startFunc_
    , haskellAppIOFunc stopFunc_
    , haskellIOFunc callbacksFunc_ ) :
      Application(context)
    , setupFunc(setupFunc_)
    , startFunc(startFunc_)
    , stopFunc(stopFunc_)
    , callbacksFunc(callbacksFunc_)
  {
    SubscribeToEvent(E_ENDFRAME, URHO3D_HANDLER(ApplicationH, HandleHaskellCallbacks));
  }

  void setEngineParameter(const char* name, Variant* value) {
    engineParameters_[name] = *value;
  }

  SharedPtr<Engine>* getEgine() {
    return new SharedPtr<Engine>(engine_);
  }

  void Setup() {
    if (setupFunc) setupFunc(this);
  }

  void Start() {
    if (startFunc) startFunc(this);
  }

  void Stop() {
    if (stopFunc) stopFunc(this);
  }

  private:
  haskellAppIOFunc setupFunc = NULL;
  haskellAppIOFunc startFunc = NULL;
  haskellAppIOFunc stopFunc = NULL;
  haskellIOFunc callbacksFunc = NULL;

  void HandleHaskellCallbacks(StringHash eventType, VariantMap& eventData) {
    if(callbacksFunc) {
      callbacksFunc();
    }
  }

};
|]

applicationContext :: C.Context
applicationContext = applicationCntx <> sharedApplicationHPtrCntx <> objectContext

newApplication :: Ptr Context
  -> (Ptr Application -> IO ()) -- ^ Setup function
  -> (Ptr Application -> IO ()) -- ^ Start function
  -> (Ptr Application -> IO ()) -- ^ Stop function
  -> IO (Ptr Application)
newApplication ptr setupFunc startFunc stopFunc = do
  [C.exp| ApplicationH* {
    new ApplicationH($(Context* ptr)
      , $funConst:(void (*setupFunc)(ApplicationH*))
      , $funConst:(void (*startFunc)(ApplicationH*))
      , $funConst:(void (*stopFunc)(ApplicationH*))
      , $funConst:(void (*runAllMainThreadCallbacks)())
      )
  } |]

deleteApplication :: Ptr Application -> IO ()
deleteApplication ptr = [C.exp| void { delete $(ApplicationH* ptr) } |]

-- | Agruments required to create new application
data ApplicationCreate = ApplicationCreate {
  appCreateContext :: Ptr Context
, appCreateSetup   :: Ptr Application -> IO ()
, appCreateStart   :: Ptr Application -> IO ()
, appCreateStop    :: Ptr Application -> IO ()
}

instance Creatable (Ptr Application) where
  type CreationOptions (Ptr Application) = ApplicationCreate

  newObject ApplicationCreate{..} = liftIO $ newApplication appCreateContext appCreateSetup appCreateStart appCreateStop
  deleteObject = liftIO . deleteApplication

deriveParent ''Object ''ApplicationH

sharedPtr "ApplicationH"

-- | Sets inital values of engine startup configuration by key-value
setStartupParameter :: (Parent Application app, Pointer p app, VariantStorable a)
  => p -- ^ Pointer to application or child
  -> String -- ^ Name of parameter
  -> a -- ^ Value of parameter
  -> IO ()
setStartupParameter p name a = withCString name $ \cname -> do
  var <- newVariant a
  let ptr = parentPointer p
  [C.exp| void { $(ApplicationH* ptr)->setEngineParameter($(char* cname), $(Variant* var)) }|]

-- | Sets inital values of engine startup configuration by key-value
startupParameter :: (Parent Application app, Pointer p app, VariantStorable a) => p -> String -> SettableStateVar a
startupParameter ptr name = makeSettableStateVar $ setStartupParameter ptr name

C.verbatim "typedef SharedPtr<Engine> SharedEngine;"

-- | Returns shared reference to inner engine
applicationEngine :: (Parent Application a, Pointer p a, MonadIO m) => p -> m (SharedPtr Engine)
applicationEngine p = liftIO $ do
  let ptr = parentPointer p
  peekSharedPtr =<< [C.exp| SharedEngine* { $(ApplicationH* ptr)->getEgine() } |]

-- | Runs application loop, doesn't exit until call to engineExit
applicationRun :: (Parent Application a, Pointer p a, MonadIO m) => p -> m ()
applicationRun p = liftIO $ do
  let ptr = parentPointer p
  [C.block| void { $(ApplicationH* ptr)->Run(); } |]
