{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Engine.Application(
    Application
  , applicationContext
  , startupParameter
  , applicationEngine
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

C.context (C.cppCtx <> applicationCntx <> contextContext <> variantContext <> objectContext <> engineContext)
C.include "<Urho3D/Engine/Engine.h>"
C.include "<Urho3D/Engine/Application.h>"
C.using "namespace Urho3D"

C.verbatim [r|
class ApplicationH : public Application {

  OBJECT(ApplicationH);

  public:

  ApplicationH(Context* context) : Application(context) {

  }

  void setEngineParameter(const char* name, Variant* value) {
    engineParameters_[name] = *value;
  }

  SharedPtr<Engine>* getEgine() {
    return new SharedPtr<Engine>(engine_);
  }
};
|]

applicationContext :: C.Context 
applicationContext = applicationCntx <> objectContext

newApplication :: Ptr Context -> IO (Ptr Application)
newApplication ptr = [C.exp| ApplicationH* { new ApplicationH($(Context* ptr)) } |]

deleteApplication :: Ptr Application -> IO ()
deleteApplication ptr = [C.exp| void { delete $(ApplicationH* ptr) } |]

instance Createable Application where 
  type CreationOptions Application = Ptr Context 

  newObject = liftIO . newApplication
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
applicationEngine :: MonadIO m => Ptr Application -> m SharedEnginePtr
applicationEngine ptr = liftIO $ [C.exp| SharedEngine* { $(ApplicationH* ptr)->getEgine() } |]