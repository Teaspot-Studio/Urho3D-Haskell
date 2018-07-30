{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Network.Network(
    Network
  , networkContext
  , networkConnect
  , networkDisconnect
  , networkStartServer
  , networkStopServer
  , networkBroadcastMessage
  , networkSetUpdateFps
  , networkSetSimulatedLatency
  , networkSetSimulatedPacketLoss
  , networkGetServerConnection
  , networkGetClientConnections
  , networkIsServerRunning
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Data.Vector (Vector)
import Foreign
import Foreign.C.String
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Network.Connection
import Graphics.Urho3D.Network.Internal.Network
import Graphics.Urho3D.Scene.Scene

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

C.context (C.cppCtx <> networkCntx <> contextContext <> sceneContext <> connectionContext)
C.include "<Urho3D/Network/Network.h>"
C.using "namespace Urho3D"

networkContext :: C.Context
networkContext = networkCntx

C.verbatim "typedef Vector<SharedPtr<Connection> > VectorSharedPtrConnection;"

-- | Connect to a server using UDP protocol. Return true if connection process successfully started.
-- bool Connect(const String& address, unsigned short port, Scene* scene, const VariantMap& identity = Variant::emptyVariantMap);
networkConnect :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> String -- ^ address
  -> Word -- ^ Port
  -> Ptr Scene -- ^ Scene
  -> m Bool
networkConnect ptr address port pscene = liftIO $ withCString address $ \address' -> do
  let ptr' = parentPointer ptr
      port' = fromIntegral port
  toBool <$> [C.exp| int { (int)$(Network* ptr')->Connect(String($(const char* address')), $(unsigned short port'), $(Scene* pscene)) } |]

-- | Disconnect the connection to the server. If wait time is non-zero, will block while waiting for disconnect to finish.
-- void Disconnect(int waitMSec = 0);
networkDisconnect :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> Int -- ^ Wait milliseconds default 0
  -> m ()
networkDisconnect ptr wmsec = liftIO $ do
  let ptr' = parentPointer ptr
      wmsec' = fromIntegral wmsec
  [C.exp| void { $(Network* ptr')->Disconnect($(int wmsec')) } |]

-- | Start a server on a port using UDP protocol. Return true if successful.
-- bool StartServer(unsigned short port);
networkStartServer :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> Word -- ^ Port
  -> m Bool
networkStartServer ptr port = liftIO $ do
  let ptr' = parentPointer ptr
      port' = fromIntegral port
  toBool <$> [C.exp| int { (int)$(Network* ptr')->StartServer($(int port')) } |]

-- | Stop the server.
-- void StopServer();
networkStopServer :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> m ()
networkStopServer ptr = liftIO $ do
  let ptr' = parentPointer ptr
  [C.exp| void { $(Network* ptr')->StopServer() } |]

-- | Broadcast a message with content ID to all client connections.
-- void BroadcastMessage
--    (int msgID, bool reliable, bool inOrder, const unsigned char* data, unsigned numBytes, unsigned contentID = 0);
networkBroadcastMessage :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> Int -- ^ Message id
  -> Bool -- ^ Reliable flag
  -> Bool -- ^ Keep order flag
  -> BS.ByteString -- ^ Buffer
  -> Word -- ^ Content ID (default 0)
  -> m ()
networkBroadcastMessage ptr msgID reliable inOrder bs contentID = liftIO $ BS.unsafeUseAsCStringLen bs $ \(bsptr, l) -> do
  let ptr' = parentPointer ptr
      msgID' = fromIntegral msgID
      reliable' = fromBool reliable
      inOrder' = fromBool inOrder
      contentID' = fromIntegral contentID
      l' = fromIntegral l
      bsptr' = castPtr bsptr
  [C.exp| void { $(Network* ptr')->BroadcastMessage($(int msgID'), $(int reliable') != 0, $(int inOrder') != 0, $(const unsigned char* bsptr'), $(unsigned int l'), $(unsigned int contentID')) } |]

-- | Broadcast a remote event to all client connections.
-- void BroadcastRemoteEvent(StringHash eventType, bool inOrder, const VariantMap& eventData = Variant::emptyVariantMap);
-- | Broadcast a remote event to all client connections in a specific scene.
-- void BroadcastRemoteEvent
--    (Scene* scene, StringHash eventType, bool inOrder, const VariantMap& eventData = Variant::emptyVariantMap);
-- | Broadcast a remote event with the specified node as a sender. Is sent to all client connections in the node's scene.
-- void BroadcastRemoteEvent
--    (Node* node, StringHash eventType, bool inOrder, const VariantMap& eventData = Variant::emptyVariantMap);

-- | Set network update FPS.
-- void SetUpdateFps(int fps);
networkSetUpdateFps :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> Int -- ^ FPS
  -> m ()
networkSetUpdateFps ptr fps = liftIO $ do
  let ptr' = parentPointer ptr
      fps' = fromIntegral fps
  [C.exp| void { $(Network* ptr')->SetUpdateFps($(int fps')) } |]

-- | Set simulated latency in milliseconds. This adds a fixed delay before sending each packet.
-- void SetSimulatedLatency(int ms);
networkSetSimulatedLatency :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> Int -- ^ ms
  -> m ()
networkSetSimulatedLatency ptr ms = liftIO $ do
  let ptr' = parentPointer ptr
      ms' = fromIntegral ms
  [C.exp| void { $(Network* ptr')->SetSimulatedLatency($(int ms')) } |]

-- | Set simulated packet loss probability between 0.0 - 1.0.
-- void SetSimulatedPacketLoss(float probability);
networkSetSimulatedPacketLoss :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> Float -- ^ probality
  -> m ()
networkSetSimulatedPacketLoss ptr pb = liftIO $ do
  let ptr' = parentPointer ptr
      pb' = realToFrac pb
  [C.exp| void { $(Network* ptr')->SetSimulatedPacketLoss($(float pb')) } |]

-- | Register a remote event as allowed to be received. There is also a fixed blacklist of events that can not be allowed in any case, such as ConsoleCommand.
-- void RegisterRemoteEvent(StringHash eventType);
-- | Unregister a remote event as allowed to received.
-- void UnregisterRemoteEvent(StringHash eventType);
-- | Unregister all remote events.
-- void UnregisterAllRemoteEvents();
-- | Set the package download cache directory.
-- void SetPackageCacheDir(const String& path);
-- | Trigger all client connections in the specified scene to download a package file from the server. Can be used to download additional resource packages when clients are already joined in the scene. The package must have been added as a requirement to the scene, or else the eventual download will fail.
-- void SendPackageToClients(Scene* scene, PackageFile* package);
-- | Perform an HTTP request to the specified URL. Empty verb defaults to a GET request. Return a request object which can be used to read the response data.
-- SharedPtr<HttpRequest> MakeHttpRequest
--    (const String& url, const String& verb = String::EMPTY, const Vector<String>& headers = Vector<String>(),
--        const String& postData = String::EMPTY);

-- | Return network update FPS.
-- int GetUpdateFps() const { return updateFps_; }

-- | Return simulated latency in milliseconds.
-- int GetSimulatedLatency() const { return simulatedLatency_; }

-- | Return simulated packet loss probability.
-- float GetSimulatedPacketLoss() const { return simulatedPacketLoss_; }

-- | Return the connection to the server. Null if not connected.
-- Connection* GetServerConnection() const;
networkGetServerConnection :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> m (Maybe (Ptr Connection))
networkGetServerConnection ptr = liftIO $ do
  let ptr' = parentPointer ptr
  wrapNullPtr <$> [C.exp| Connection* { $(Network* ptr')->GetServerConnection() } |]

-- | Return all client connections.
-- Vector<SharedPtr<Connection> > GetClientConnections() const;
networkGetClientConnections :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> m (Vector (SharedPtr Connection))
networkGetClientConnections ptr = liftIO $ do
  let ptr' = parentPointer ptr
  resptr <- [C.exp| VectorSharedPtrConnection* {
    new VectorSharedPtrConnection($(Network* ptr')->GetClientConnections()) }
  |]
  v <- peekForeignVectorAs resptr
  [C.exp| void { delete $(VectorSharedPtrConnection* resptr) }|]
  pure v

-- | Return whether the server is running.
-- bool IsServerRunning() const;
networkIsServerRunning :: (Parent Network a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Network' or ancestor
  -> m Bool
networkIsServerRunning ptr = liftIO $ do
  let ptr' = parentPointer ptr
  toBool <$> [C.exp| int { (int)$(Network* ptr')->IsServerRunning() } |]

-- | Return whether a remote event is allowed to be received.
-- bool CheckRemoteEvent(StringHash eventType) const;

-- | Return the package download cache directory.
-- const String& GetPackageCacheDir() const { return packageCacheDir_; }
