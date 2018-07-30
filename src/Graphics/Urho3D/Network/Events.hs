module Graphics.Urho3D.Network.Events(
    EventServerConnected(..)
  , EventServerDisconnected(..)
  , EventConnectFailed(..)
  , EventClientConnected(..)
  , EventClientDisconnected(..)
  , EventClientIdentity(..)
  , EventClientSceneLoaded(..)
  , EventNetworkMessage(..)
  , EventNetworkUpdate(..)
  , EventNetworkUpdateSent(..)
  , EventNetworkSceneLoadFailed(..)
  , EventRemoteEventData(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Maybe
import Data.Monoid
import Foreign
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Network.Connection

import qualified Data.ByteString as BS

C.context (C.cppCtx <> stringHashContext <> connectionContext)
C.include "<Urho3D/Network/NetworkEvents.h>"
C.using "namespace Urho3D"

-- | Server connection established.
data EventServerConnected = EventServerConnected {
} deriving (Show)

instance Event EventServerConnected where
  eventID _ = [C.pure| const StringHash* {&E_SERVERCONNECTED} |]
  loadEventData _ = pure EventServerConnected

-- | Server connection disconnected.
data EventServerDisconnected = EventServerDisconnected {
} deriving (Show)

instance Event EventServerDisconnected where
  eventID _ = [C.pure| const StringHash* {&E_SERVERDISCONNECTED} |]
  loadEventData _ = pure EventServerDisconnected

-- | Server connection failed.
data EventConnectFailed = EventConnectFailed {
} deriving (Show)

instance Event EventConnectFailed where
  eventID _ = [C.pure| const StringHash* {&E_CONNECTFAILED} |]
  loadEventData _ = pure EventConnectFailed

-- | New client connection established.
data EventClientConnected = EventClientConnected {
  eventClientConnectedConnection :: Ptr Connection
} deriving (Show)

instance Event EventClientConnected where
  eventID _ = [C.pure| const StringHash* {&E_CLIENTCONNECTED} |]
  loadEventData vmap = do
    conn :: Maybe (Ptr Connection) <- variantMapGet' vmap [C.pure| const StringHash* {&ClientConnected::P_CONNECTION} |]
    pure EventClientConnected {
        eventClientConnectedConnection = fromMaybe (error "EventClientConnected got null connection!") conn
      }

-- | Client connection disconnected.
data EventClientDisconnected = EventClientDisconnected {
  eventClientDisconnectedConnection :: Ptr Connection
} deriving (Show)

instance Event EventClientDisconnected where
  eventID _ = [C.pure| const StringHash* {&E_CLIENTDISCONNECTED} |]
  loadEventData vmap = do
    conn :: Maybe (Ptr Connection) <- variantMapGet' vmap [C.pure| const StringHash* {&ClientDisconnected::P_CONNECTION} |]
    pure EventClientDisconnected {
        eventClientDisconnectedConnection = fromMaybe (error "EventClientDisconnected got null connection!") conn
      }

-- | Client has sent identity: identity map is in the event data.
data EventClientIdentity = EventClientIdentity {
  eventClientIdentityConnection :: Ptr Connection
, eventClientIdentityAllow      :: Bool
, eventClientIdentityData       :: Ptr VariantMap
} deriving (Show)

instance Event EventClientIdentity where
  eventID _ = [C.pure| const StringHash* {&E_CLIENTIDENTITY} |]
  loadEventData vmap = do
    conn :: Maybe (Ptr Connection) <- variantMapGet' vmap [C.pure| const StringHash* {&ClientIdentity::P_CONNECTION} |]
    allow :: Maybe Bool <- variantMapGet' vmap [C.pure| const StringHash* {&ClientIdentity::P_ALLOW} |]
    pure EventClientIdentity {
        eventClientIdentityConnection = fromMaybe (error "EventClientIdentity got null connection!") conn
      , eventClientIdentityAllow = fromMaybe False allow
      , eventClientIdentityData = vmap
      }

-- | Client has informed to have loaded the scene.
data EventClientSceneLoaded = EventClientSceneLoaded {
  eventClientSceneLoadedConnection :: Ptr Connection
} deriving (Show)

instance Event EventClientSceneLoaded where
  eventID _ = [C.pure| const StringHash* {&E_CLIENTSCENELOADED} |]
  loadEventData vmap = do
    conn :: Maybe (Ptr Connection) <- variantMapGet' vmap [C.pure| const StringHash* {&ClientSceneLoaded::P_CONNECTION} |]
    pure EventClientSceneLoaded {
        eventClientSceneLoadedConnection = fromMaybe (error "EventClientSceneLoaded got null connection!") conn
      }

-- | Unhandled network message received.
data EventNetworkMessage = EventNetworkMessage {
  eventNetworkMessageConnection :: Ptr Connection
, eventNetworkMessageId         :: Int
, eventNetworkMessageData       :: BS.ByteString
} deriving (Show)

instance Event EventNetworkMessage where
  eventID _ = [C.pure| const StringHash* {&E_NETWORKMESSAGE} |]
  loadEventData vmap = do
    conn :: Maybe (Ptr Connection) <- variantMapGet' vmap [C.pure| const StringHash* {&NetworkMessage::P_CONNECTION} |]
    mid :: Maybe Int <- variantMapGet' vmap [C.pure| const StringHash* {&NetworkMessage::P_MESSAGEID} |]
    buff :: Maybe BS.ByteString <- variantMapGet' vmap [C.pure| const StringHash* {&NetworkMessage::P_DATA} |]
    pure EventNetworkMessage {
        eventNetworkMessageConnection = fromMaybe (error "EventNetworkMessage got null connection!") conn
      , eventNetworkMessageId = fromMaybe 0 mid
      , eventNetworkMessageData = fromMaybe mempty buff
      }

-- | About to send network update on the client or server.
data EventNetworkUpdate = EventNetworkUpdate {
} deriving (Show)

instance Event EventNetworkUpdate where
  eventID _ = [C.pure| const StringHash* {&E_NETWORKUPDATE} |]
  loadEventData _ = pure EventNetworkUpdate

-- | Network update has been sent on the client or server.
data EventNetworkUpdateSent = EventNetworkUpdateSent {
} deriving (Show)

instance Event EventNetworkUpdateSent where
  eventID _ = [C.pure| const StringHash* {&E_NETWORKUPDATESENT} |]
  loadEventData _ = pure EventNetworkUpdateSent

-- | Scene load failed, either due to file not found or checksum error.
data EventNetworkSceneLoadFailed = EventNetworkSceneLoadFailed {
  eventNetworkSceneLoadFailedConnection :: Ptr Connection
} deriving (Show)

instance Event EventNetworkSceneLoadFailed where
  eventID _ = [C.pure| const StringHash* {&E_NETWORKSCENELOADFAILED} |]
  loadEventData vmap = do
    conn :: Maybe (Ptr Connection) <- variantMapGet' vmap [C.pure| const StringHash* {&NetworkSceneLoadFailed::P_CONNECTION} |]
    pure EventNetworkSceneLoadFailed {
        eventNetworkSceneLoadFailedConnection = fromMaybe (error "EventNetworkSceneLoadFailed got null connection!") conn
      }

-- | Remote event: adds Connection parameter to the event data
data EventRemoteEventData = EventRemoteEventData {
  eventRemoteEventDataConnection :: Ptr Connection
, eventRemoteEventDataData       :: Ptr VariantMap
} deriving (Show)

instance Event EventRemoteEventData where
  eventID _ = [C.pure| const StringHash* {&E_REMOTEEVENTDATA} |]
  loadEventData vmap = do
    conn :: Maybe (Ptr Connection) <- variantMapGet' vmap [C.pure| const StringHash* {&RemoteEventData::P_CONNECTION} |]
    pure EventRemoteEventData {
        eventRemoteEventDataConnection = fromMaybe (error "EventRemoteEventData got null connection!") conn
      , eventRemoteEventDataData = vmap
      }
