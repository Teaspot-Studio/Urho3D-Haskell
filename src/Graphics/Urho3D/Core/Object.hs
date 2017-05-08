{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.Object(
    Object
  , SharedObject
  , objectContext
  , getSubsystem
  , Subsystem(..)
  , Event(..)
  , subscribeToEvent
  , subscribeToEventSpecific
  , unsubscribeFromEvent
  , unsubscribeFromEventSpecific
  , getContext
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.HashMap.Strict as M

import Graphics.Urho3D.Core.Internal.Object
import Graphics.Urho3D.Core.Internal.Context
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Text.RawString.QQ
import Data.Monoid
import Foreign
import Data.Proxy
import Data.Hashable
import System.IO.Unsafe (unsafePerformIO)

C.context (C.cppCtx <> C.funConstCtx <> objectCntx <> contextCntx <> stringHashContext <> variantContext <> sharedObjectPtrCntx)
C.include "<Urho3D/Core/Object.h>"
C.using "namespace Urho3D"

objectContext :: C.Context
objectContext = objectCntx <> stringHashContext <> sharedObjectPtrCntx

sharedPtr "Object"

-- | A subsystem of Urho3D, that can be aquired by Object API
class Subsystem a where
  getSubsystemImpl :: Ptr Object -> IO (Ptr a)

-- | Returns specified subsystem from Object Context
getSubsystem :: (MonadIO m, Subsystem a, Parent Object b, Pointer p b) => p -> m (Maybe (Ptr a))
getSubsystem ptr = do
  res <- liftIO $ getSubsystemImpl (parentPointer ptr)
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
  URHO3D_OBJECT(HaskellHandler, Object);

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

-- | Stores all haskell handlers for events to be able to delete them after
handlerMap :: TVar (M.HashMap (Ptr Object, Ptr StringHash, Maybe (Ptr Object)) (Ptr HaskellHandler))
handlerMap = unsafePerformIO $ newTVarIO M.empty

-- | Creates new haskell handler for event
newHaskellHandler :: Ptr Object -- ^ Reciever pointer
  -> Ptr StringHash -- ^ Event type
  -> Maybe (Ptr Object) -- ^ Maybe a sender (Specific subscribe variantion)
  -> (Ptr HashMapStringHashVariant -> IO ()) -- ^ Actual handler
  -> IO (Ptr HaskellHandler) -- ^ Object that is tracked in handler map
newHaskellHandler receiver event sender funImpl = do
  handler <- [C.block| HaskellHandler* {
    Context* cntx = $(Object* receiver)->GetContext();
    return new HaskellHandler(cntx, $funConst:(void (*funImpl)(HashMapStringHashVariant*)) );
  } |]
  freeHaskellHandler receiver event sender
  addHaskellHandler receiver event sender handler
  return handler

-- | Deletes haskell handler with specified receiver, event type and sender (maybe)
freeHaskellHandler :: Ptr Object -- ^ Reciever pointer
  -> Ptr StringHash -- ^ Event type
  -> Maybe (Ptr Object) -- ^ Maybe a sender (Specific subscribe variantion)
  -> IO ()
freeHaskellHandler receiver event sender = do
  mhandler <- atomically $ do
    let k = (receiver, event, sender)
    mhandler <- M.lookup k <$> readTVar handlerMap
    modifyTVar' handlerMap $ M.delete k
    return mhandler

  case mhandler of
    Just handler -> [C.exp| void { delete $(HaskellHandler* handler) } |]
    Nothing -> return ()

-- | Registers haskell handler in map
addHaskellHandler :: Ptr Object -- ^ Reciever pointer
  -> Ptr StringHash -- ^ Event type
  -> Maybe (Ptr Object) -- ^ Maybe a sender (Specific subscribe variantion)
  -> Ptr HaskellHandler
  -> IO ()
addHaskellHandler receiver event sender handler = atomically $ modifyTVar' handlerMap $ M.insert (receiver, event, sender) handler

-- | Binds function to specific event
subscribeToEvent :: forall m event a p . (Parent Object a, Pointer p a, MonadIO m, Event event) => p -> (event -> IO ()) -> m ()
subscribeToEvent obj fun = do
  -- Actual handler
  let funImpl vmap = fun =<< loadEventData vmap
      objPtr = parentPointer obj
      eventType = eventID (Proxy :: Proxy event)
  liftIO $ do
    handler <- newHaskellHandler objPtr eventType Nothing funImpl
    [C.block| void {
    $(Object* objPtr)->SubscribeToEvent(*$(StringHash* eventType)
        , new EventHandlerImpl<HaskellHandler>($(HaskellHandler* handler), &HaskellHandler::runHanlder)
        );
    } |]

-- | Subscribe to a specific sender's event.
subscribeToEventSpecific :: forall m event a b p pSender . (Parent Object a, Pointer p a, Parent Object b, Pointer pSender b, MonadIO m, Event event)
  => p -> pSender -> (event -> IO ()) -> m ()
subscribeToEventSpecific obj sender fun = do
  -- Actual handler
  let funImpl vmap = fun =<< loadEventData vmap
      objPtr = parentPointer obj
      senderPtr = parentPointer sender
      eventType = eventID (Proxy :: Proxy event)
  liftIO $ do
    handler <- newHaskellHandler objPtr eventType (Just senderPtr) funImpl
    [C.block| void {
    $(Object* objPtr)->SubscribeToEvent($(Object* senderPtr), *$(StringHash* eventType)
        , new EventHandlerImpl<HaskellHandler>($(HaskellHandler* handler), &HaskellHandler::runHanlder)
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
  freeHaskellHandler ptr eventType Nothing

-- | Unbinds function from specific event from specific sender
unsubscribeFromEventSpecific :: (Parent Object a, Pointer p a, Parent Object b, Pointer pSender b, Event event, MonadIO m)
  => p -- ^ Pointer to object or child
  -> pSender -- ^ Pointer to sender object
  -> Proxy event -- ^ Event type
  -> m ()
unsubscribeFromEventSpecific p sender event = liftIO $ do
  let ptr = parentPointer p
      eventType = eventID event
      senderPtr = parentPointer sender
  [C.exp| void { $(Object* ptr)->UnsubscribeFromEvent($(Object* senderPtr), *$(StringHash* eventType)) }|]
  freeHaskellHandler ptr eventType (Just senderPtr)

-- | Returns inner Urho context of object
getContext :: (Parent Object a, Pointer p a, MonadIO m) => p -> m (Ptr Context)
getContext ptr = liftIO $ do
  let objPtr = parentPointer ptr
  [C.exp| Context* { $(Object* objPtr)->GetContext() } |]
