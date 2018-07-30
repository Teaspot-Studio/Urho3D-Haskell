{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Network.Connection(
    Connection
  , SharedConnection
  , VectorSharedPtrConnection
  , connectionContext
  , connectionSendMessage
  , connectionDisconnect
  , connectionIsClient
  , connectionIsConnected
  , connectionIsConnectPending
  , connectionIsSceneLoaded
  , connectionGetLogStatistics
  , connectionGetAddress
  , connectionGetPort
  , connectionGetRoundTripTime
  , connectionGetLastHeardTime
  , connectionGetBytesInPerSec
  , connectionGetBytesOutPerSec
  , connectionGetPacketsInPerSec
  , connectionGetPacketsOutPerSec
  , connectionToString
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Foreign.C.String
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Network.Internal.Connection

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

C.context (C.cppCtx <> connectionCntx <> contextContext <> sharedConnectionPtrCntx)
C.include "<Urho3D/Network/Connection.h>"
C.using "namespace Urho3D"

connectionContext :: C.Context
connectionContext = connectionCntx <> sharedConnectionPtrCntx

sharedPtr "Connection"

C.verbatim "typedef Vector<SharedPtr<Connection> > VectorSharedPtrConnection;"

instance Creatable (Ptr VectorSharedPtrConnection) where
  type CreationOptions (Ptr VectorSharedPtrConnection) = ()
  newObject _ = liftIO [C.exp| VectorSharedPtrConnection* {new Vector<SharedPtr<Connection> >() } |]
  deleteObject ptr = liftIO [C.exp| void { delete $(VectorSharedPtrConnection* ptr) } |]

instance ReadableVector VectorSharedPtrConnection where
  type ReadVecElem VectorSharedPtrConnection = SharedPtr Connection
  foreignVectorLength ptr = liftIO $ fromIntegral <$> [C.exp| int {$(VectorSharedPtrConnection* ptr)->Size() } |]
  foreignVectorElement ptr i = liftIO $ peekSharedPtr =<< [C.exp| SharedConnection* { new SharedPtr<Connection>((*$(VectorSharedPtrConnection* ptr))[$(unsigned int i')]) } |]
    where i' = fromIntegral i

instance WriteableVector VectorSharedPtrConnection where
  type WriteVecElem VectorSharedPtrConnection = SharedPtr Connection
  foreignVectorAppend ptr e = liftIO $ [C.exp| void {$(VectorSharedPtrConnection* ptr)->Push(SharedPtr<Connection>($(Connection* e'))) } |]
    where e' = parentPointer e

-- | Send a message.
-- void SendMessage(int msgID, bool reliable, bool inOrder, const unsigned char* data, unsigned numBytes, unsigned contentID = 0);
connectionSendMessage :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> Int -- ^ Message id
  -> Bool -- ^ Reliable flag
  -> Bool -- ^ Keep order flag
  -> BS.ByteString -- ^ Buffer
  -> Word -- ^ Content ID (default 0)
  -> m ()
connectionSendMessage ptr msgID reliable inOrder bs contentID = liftIO $ BS.unsafeUseAsCStringLen bs $ \(bsptr, l) -> do
  let ptr' = parentPointer ptr
      msgID' = fromIntegral msgID
      reliable' = fromBool reliable
      inOrder' = fromBool inOrder
      contentID' = fromIntegral contentID
      l' = fromIntegral l
      bsptr' = castPtr bsptr
  [C.exp| void { $(Connection* ptr')->SendMessage($(int msgID'), $(int reliable') != 0, $(int inOrder') != 0, $(const unsigned char* bsptr'), $(unsigned int l'), $(unsigned int contentID')) } |]

-- | Send a remote event.
-- void SendRemoteEvent(StringHash eventType, bool inOrder, const VariantMap& eventData = Variant::emptyVariantMap);
-- | Send a remote event with the specified node as sender.
-- void SendRemoteEvent(Node* node, StringHash eventType, bool inOrder, const VariantMap& eventData = Variant::emptyVariantMap);
-- | Assign scene. On the server, this will cause the client to load it.
-- void SetScene(Scene* newScene);
-- | Set new controls.
-- void SetControls(const Controls& newControls);
-- | Set the observer position for interest management, to be sent to the server.
-- void SetPosition(const Vector3& position);
-- | Set the observer rotation for interest management, to be sent to the server. Note: not used by the NetworkPriority component.
-- void SetRotation(const Quaternion& rotation);
-- | Set whether to log data in/out statistics.
-- void SetLogStatistics(bool enable);

-- | Disconnect. If wait time is non-zero, will block while waiting for disconnect to finish.
-- void Disconnect(int waitMSec = 0);
connectionDisconnect :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> Int -- ^ Wait milliseconds
  -> m ()
connectionDisconnect ptr waitMSec = liftIO $ do
  let ptr' = parentPointer ptr
      waitMSec' = fromIntegral waitMSec
  [C.exp| void { $(Connection* ptr')->Disconnect($(int waitMSec')) } |]

-- | Return the scene used by this connection.
-- Scene* GetScene() const;

-- | Return the client controls of this connection.
-- const Controls& GetControls() const { return controls_; }

-- | Return the controls timestamp, sent from client to server along each control update.
-- unsigned char GetTimeStamp() const { return timeStamp_; }

-- | Return the observer position sent by the client for interest management.
-- const Vector3& GetPosition() const { return position_; }

-- | Return the observer rotation sent by the client for interest management.
-- const Quaternion& GetRotation() const { return rotation_; }

-- | Return whether is a client connection.
-- bool IsClient() const { return isClient_; }
connectionIsClient :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Bool
connectionIsClient ptr = liftIO $ do
  let ptr' = parentPointer ptr
  toBool <$> [C.exp| int { (int)$(Connection* ptr')->IsClient() } |]

-- | Return whether is fully connected.
-- bool IsConnected() const;
connectionIsConnected :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Bool
connectionIsConnected ptr = liftIO $ do
  let ptr' = parentPointer ptr
  toBool <$> [C.exp| int { (int)$(Connection* ptr')->IsConnected() } |]

-- | Return whether connection is pending.
-- bool IsConnectPending() const { return connectPending_; }
connectionIsConnectPending :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Bool
connectionIsConnectPending ptr = liftIO $ do
  let ptr' = parentPointer ptr
  toBool <$> [C.exp| int { (int)$(Connection* ptr')->IsConnectPending() } |]

-- | Return whether the scene is loaded and ready to receive server updates.
-- bool IsSceneLoaded() const { return sceneLoaded_; }
connectionIsSceneLoaded :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Bool
connectionIsSceneLoaded ptr = liftIO $ do
  let ptr' = parentPointer ptr
  toBool <$> [C.exp| int { (int)$(Connection* ptr')->IsSceneLoaded() } |]

-- | Return whether to log data in/out statistics.
-- bool GetLogStatistics() const { return logStatistics_; }
connectionGetLogStatistics :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Bool
connectionGetLogStatistics ptr = liftIO $ do
  let ptr' = parentPointer ptr
  toBool <$> [C.exp| int { (int)$(Connection* ptr')->GetLogStatistics() } |]

-- | Return remote address.
-- String GetAddress() const { return address_; }
connectionGetAddress :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m String
connectionGetAddress ptr = liftIO $ do
  let ptr' = parentPointer ptr
  peekCString =<< [C.exp| const char* { $(Connection* ptr')->GetAddress().CString() } |]

-- | Return remote port.
-- unsigned short GetPort() const { return port_; }
connectionGetPort :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Word
connectionGetPort ptr = liftIO $ do
  let ptr' = parentPointer ptr
  fromIntegral <$> [C.exp| unsigned short { $(Connection* ptr')->GetPort() } |]

-- | Return the connection's round trip time in milliseconds.
-- float GetRoundTripTime() const;
connectionGetRoundTripTime :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Float
connectionGetRoundTripTime ptr = liftIO $ do
  let ptr' = parentPointer ptr
  realToFrac <$> [C.exp| float { $(Connection* ptr')->GetRoundTripTime() } |]

-- | Return the time since last received data from the remote host in milliseconds.
-- float GetLastHeardTime() const;
connectionGetLastHeardTime :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Float
connectionGetLastHeardTime ptr = liftIO $ do
  let ptr' = parentPointer ptr
  realToFrac <$> [C.exp| float { $(Connection* ptr')->GetLastHeardTime() } |]

-- | Return bytes received per second.
-- float GetBytesInPerSec() const;
connectionGetBytesInPerSec :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Float
connectionGetBytesInPerSec ptr = liftIO $ do
  let ptr' = parentPointer ptr
  realToFrac <$> [C.exp| float { $(Connection* ptr')->GetBytesInPerSec() } |]

-- | Return bytes sent per second.
-- float GetBytesOutPerSec() const;
connectionGetBytesOutPerSec :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Float
connectionGetBytesOutPerSec ptr = liftIO $ do
  let ptr' = parentPointer ptr
  realToFrac <$> [C.exp| float { $(Connection* ptr')->GetBytesOutPerSec() } |]

-- | Return packets received per second.
-- float GetPacketsInPerSec() const;
connectionGetPacketsInPerSec :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Float
connectionGetPacketsInPerSec ptr = liftIO $ do
  let ptr' = parentPointer ptr
  realToFrac <$> [C.exp| float { $(Connection* ptr')->GetPacketsInPerSec() } |]

-- | Return packets sent per second.
-- float GetPacketsOutPerSec() const;
connectionGetPacketsOutPerSec :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m Float
connectionGetPacketsOutPerSec ptr = liftIO $ do
  let ptr' = parentPointer ptr
  realToFrac <$> [C.exp| float { $(Connection* ptr')->GetPacketsOutPerSec() } |]

-- | Return an address:port string.
-- String ToString() const;
connectionToString :: (Parent Connection a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'Connection' or ancestor
  -> m String
connectionToString ptr = liftIO $ do
  let ptr' = parentPointer ptr
  peekCString =<< [C.exp| const char* { $(Connection* ptr')->ToString().CString() } |]


-- | Return number of package downloads remaining.
-- unsigned GetNumDownloads() const;
-- | Return name of current package download, or empty if no downloads.
-- const String& GetDownloadName() const;
-- | Return progress of current package download, or 1.0 if no downloads.
-- float GetDownloadProgress() const;
-- | Trigger client connection to download a package file from the server. Can be used to download additional resource packages when client is already joined in a scene. The package must have been added as a requirement to the scene the client is joined in, or else the eventual download will fail.
-- void SendPackageToClient(PackageFile* package);
