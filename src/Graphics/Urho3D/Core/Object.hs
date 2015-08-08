{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.Object(
    Object
  , objectContext
  , getSubsystem
  , Subsystem(..)
  , Event(..)
  , subscribeToEvent
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

C.context (C.cppCtx <> C.funCtx <> objectCntx <> contextContext <> stringHashContext <> variantContext)
C.include "<Urho3D/Core/Object.h>"
C.using "namespace Urho3D"

objectContext :: C.Context 
objectContext = objectCntx <> stringHashContext

-- | A subsystem of Urho3D, that can be aquired by Object API
class Subsystem a where 
  getSubsystemImpl :: Ptr Object -> IO (Ptr a)

-- | Returns specified subsystem from Object Context
getSubsystem :: (Subsystem a, Parent Object b) => Ptr b -> IO (Maybe (Ptr a))
getSubsystem ptr = do
  res <- getSubsystemImpl (castToParent ptr)
  checkNullPtr' res return

-- | Describes events in Urho3D engine
class Event a where 
  -- | Actual data that is binded to event when it fires
  data EventData a :: * 

  -- | Returns id of event for binding to it
  eventID :: Event a => a -> Ptr StringHash
  -- | How to extract event data from C++ side to Haskell
  loadEventData :: Ptr VariantMap -> IO (EventData a)

C.verbatim [r|

typedef HashMap<StringHash, Variant> HashMapStringHashVariant;

class HaskellHandler : public Object {
  OBJECT(HaskellHandler);

  typedef void (*Handler)(HashMapStringHashVariant*);
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
subscribeToEvent :: (Event a, Parent Object b) => Ptr b -> a -> (EventData a -> IO ()) -> IO ()
subscribeToEvent obj e fun = do 
  -- Actual handler
  let funImpl vmap = fun =<< loadEventData vmap

      objPtr = castToParent obj
      eventType = eventID e
  [C.block| void {
    Context* cntx = $(Object* objPtr)->GetContext();
    HaskellHandler* handler = new HaskellHandler(cntx, $fun:(void (*funImpl)(HashMapStringHashVariant* vm)));
    $(Object* objPtr)->SubscribeToEvent(*$(StringHash* eventType)
        , new EventHandlerImpl<HaskellHandler>(handler, &HaskellHandler::runHanlder)
        );
  } |]