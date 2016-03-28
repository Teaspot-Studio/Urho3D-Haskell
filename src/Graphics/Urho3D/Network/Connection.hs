{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Network.Connection(
    Connection 
  , connectionContext
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Network.Internal.Connection
import Graphics.Urho3D.Core.Context 
--import Graphics.Urho3D.Createable
--import Graphics.Urho3D.Monad
import Data.Monoid
--import Foreign 

C.context (C.cppCtx <> connectionCntx <> contextContext)
--C.include "<Urho3D/Network/Connection.h>"
--C.using "namespace Urho3D"
-- Temp to force creation of cpp file
C.include "<Urho3D/Math/Vector3.h>"

connectionContext :: C.Context 
connectionContext = connectionCntx

{-
newConnection :: Ptr Context -> IO (Ptr Connection)
newConnection ptr = [C.exp| Connection* { new Connection( $(Context* ptr) ) } |]

deleteConnection :: Ptr Connection -> IO ()
deleteConnection ptr = [C.exp| void { delete $(Connection* ptr) } |]

instance Createable (Ptr Connection) where 
  type CreationOptions (Ptr Connection) = Ptr Context 

  newObject = liftIO . newConnection
  deleteObject = liftIO . deleteConnection
-}