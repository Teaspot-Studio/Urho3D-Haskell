{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.Object(
    Object
  , objectContext
  , getSubsystem
  , Subsystem(..)
  , Event(..)
  , subscribeToEvent
  , unsubscribeFromEvent
  , getContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.Object
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Text.RawString.QQ
import Data.Monoid
import Foreign 
import Data.Proxy

C.context (C.cppCtx <> C.funConstCtx <> objectCntx <> contextContext <> stringHashContext <> variantContext)
C.include "<Urho3D/Core/Object.h>"
C.using "namespace Urho3D"

objectContext :: C.Context 
objectContext = objectCntx <> stringHashContext

-- | A subsystem of Urho3D, that can be aquired by Object API
class Subsystem a where 
  getSubsystemImpl :: Ptr Object -> IO (Ptr a)

-- | Returns specified subsystem from Object Context
getSubsystem :: (MonadIO m, Subsystem a, Parent Object b) => Ptr b -> m (Maybe (Ptr a))
getSubsystem ptr = do
  res <- liftIO $ getSubsystemImpl (castToParent ptr)
  checkNullPtr' res return

-- | Describes events in Urho3D engine
class Event event where 
  -- | Returns id of event for binding to it
  eventID :: Proxy event -> Ptr StringHash
  -- | How to extract event data from C++ side to Haskell
  loadEventData :: Ptr VariantMap -> IO event

C.verbatim [r|

typedef HashMap<StringHash, Variant> HashMapStringHashVariant;
extern "C" typedef void (*Handler)(HashMapStringHashVariant*);

class HaskellHandler : public Object {
  OBJECT(HaskellHandler);

  Handler handler_;
public:
  HaskellHandler(Context* cntx, Handler handler) : 
      Object(cntx)
    , handler_(handler) 
  {
  }

  void runHanlder(StringHash, VariantMap& vm) {
    handler_(&vm);
  }
};
|]

-- | Binds function to specific event
subscribeToEvent :: forall m event a p . (Parent Object a, Pointer p a, MonadIO m, Event event) => p -> (event -> IO ()) -> m ()
subscribeToEvent obj fun = do 
  -- Actual handler
  let funImpl vmap = fun =<< loadEventData vmap
      objPtr = parentPointer obj 
      eventType = eventID (Proxy :: Proxy event)
  liftIO $ [C.block| void {
    Context* cntx = $(Object* objPtr)->GetContext();
    HaskellHandler* handler = new HaskellHandler(cntx, $funConst:(void (*funImpl)(HashMapStringHashVariant* vm)));
    $(Object* objPtr)->SubscribeToEvent(*$(StringHash* eventType)
        , new EventHandlerImpl<HaskellHandler>(handler, &HaskellHandler::runHanlder)
        );
  } |]

-- | Unbinds function from specific event
unsubscribeFromEvent :: (Parent Object a, Pointer p a, Event event, MonadIO m) => p -- ^ Pointer to object or child
  -> Proxy event -- ^ Event type
  -> m ()
unsubscribeFromEvent p event = liftIO $ do 
  let ptr = parentPointer p 
      eventType = eventID event
  [C.exp| void { $(Object* ptr)->UnsubscribeFromEvent(*$(StringHash* eventType)) }|]

-- | Returns inner Urho context of object
getContext :: (Parent Object a, MonadIO m) => Ptr a -> m (Ptr Context)
getContext ptr = liftIO $ do 
  let objPtr = castToParent ptr 
  [C.exp| Context* { $(Object* objPtr)->GetContext() } |]