{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Engine.Application(
    Application
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
import Graphics.Urho3D.Engine.Engine
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Data.StateVar
import Text.RawString.QQ
import Foreign 
import Foreign.C.String 

C.context (C.cppCtx <> C.funConstCtx <> applicationCntx <> contextContext <> variantContext <> objectContext <> engineContext)
C.include "<Urho3D/Engine/Engine.h>"
C.include "<Urho3D/Engine/Application.h>"
C.include "<iostream>"
C.using "namespace Urho3D"

C.verbatim [r|
extern "C" typedef void (*haskellIOFunc)();

class ApplicationH : public Application {

  OBJECT(ApplicationH);

  public:

  ApplicationH(Context* context
    , haskellIOFunc setupFunc_
    , haskellIOFunc startFunc_
    , haskellIOFunc stopFunc_ ) : 
      Application(context)
    , setupFunc(setupFunc_)
    , startFunc(startFunc_)
    , stopFunc(stopFunc_)
  {

  }

  void setEngineParameter(const char* name, Variant* value) {
    engineParameters_[name] = *value;
  }

  SharedPtr<Engine>* getEgine() {
    return new SharedPtr<Engine>(engine_);
  }

  void Setup() {
    if (setupFunc) setupFunc();
  }

  void Start() {
    if (startFunc) startFunc();
  }

  void Stop() {
    if (stopFunc) stopFunc();
  }

  private:
  haskellIOFunc setupFunc = NULL;
  haskellIOFunc startFunc = NULL;
  haskellIOFunc stopFunc = NULL;
};
|]

applicationContext :: C.Context 
applicationContext = applicationCntx <> objectContext

newApplication :: Ptr Context
  -> IO () -- ^ Setup function
  -> IO () -- ^ Start function
  -> IO () -- ^ Stop function
  -> IO (Ptr Application)
newApplication ptr setupFunc startFunc stopFunc = do 
  [C.exp| ApplicationH* { 
    new ApplicationH($(Context* ptr)
      , $funConst:(void (*setupFunc)())
      , $funConst:(void (*startFunc)())
      , $funConst:(void (*stopFunc)()) )
  } |]

deleteApplication :: Ptr Application -> IO ()
deleteApplication ptr = [C.exp| void { delete $(ApplicationH* ptr) } |]

instance Createable (Ptr Application) where 
  type CreationOptions (Ptr Application) = (Ptr Context, IO (), IO (), IO ())

  newObject (cntx, setup, start, stop) = liftIO $ newApplication cntx setup start stop
  deleteObject = liftIO . deleteApplication

instance Parent Object Application where 
  castToParent ptr = [C.pure| Object* {(Object*)$(ApplicationH* ptr)} |]
  castToChild ptr = let 
    child = [C.pure| ApplicationH* {(ApplicationH*)$(Object* ptr)} |]
    in if child == nullPtr then Nothing else Just child 

-- | Sets inital values of engine startup configuration by key-value
setStartupParameter :: VariantStorable a => Ptr Application -> String -> a -> IO ()
setStartupParameter ptr name a = withCString name $ \cname -> do 
  var <- newVariant a 
  [C.exp| void { $(ApplicationH* ptr)->setEngineParameter($(char* cname), $(Variant* var)) }|]

-- | Sets inital values of engine startup configuration by key-value
startupParameter :: VariantStorable a => Ptr Application -> String -> SettableStateVar a 
startupParameter ptr name = makeSettableStateVar $ setStartupParameter ptr name

C.verbatim "typedef SharedPtr<Engine> SharedEngine;"

-- | Returns shared reference to inner engine
applicationEngine :: (Parent Application a, Pointer p a, MonadIO m) => p -> m SharedEnginePtr
applicationEngine p = liftIO $ do
  let ptr = parentPointer p
  wrapSharedEnginePtr =<< [C.exp| SharedEngine* { $(ApplicationH* ptr)->getEgine() } |]

-- | Runs application loop, doesn't exit until call to engineExit
applicationRun :: (Parent Application a, Pointer p a, MonadIO m) => p -> m ()
applicationRun p = liftIO $ do 
  let ptr = parentPointer p
  [C.block| void { $(ApplicationH* ptr)->Run(); } |]