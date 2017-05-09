{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Resource.XMLFile(
    XMLFile
  , xmlFileContext
  , xmlFileFromString
  , xmlFilePatch
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Control.Monad.IO.Class
import Data.Monoid
import Foreign
import Foreign.C.String
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Resource.Internal.XMLFile
import Graphics.Urho3D.Resource.Resource

C.context (C.cppCtx <> xmlFileCntx <> contextContext <> resourceContext)
C.include "<Urho3D/Resource/XMLFile.h>"
C.using "namespace Urho3D"

xmlFileContext :: C.Context
xmlFileContext = xmlFileCntx <> resourceContext

newXMLFile :: Ptr Context -> IO (Ptr XMLFile)
newXMLFile ptr = [C.exp| XMLFile* { new XMLFile( $(Context* ptr) ) } |]

deleteXMLFile :: Ptr XMLFile -> IO ()
deleteXMLFile ptr = [C.exp| void { delete $(XMLFile* ptr) } |]

instance Creatable (Ptr XMLFile) where
  type CreationOptions (Ptr XMLFile) = Ptr Context

  newObject = liftIO . newXMLFile
  deleteObject = liftIO . deleteXMLFile

instance ResourceType XMLFile where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { XMLFile::GetTypeStatic().Value() } |]

-- | Loads XML from string
xmlFileFromString :: MonadIO m => Ptr XMLFile -> String -> m Bool
xmlFileFromString ptr s = liftIO $ withCString s $ \s' -> do
  toBool <$> [C.exp| int { (int)$(XMLFile* ptr)->FromString(String($(const char* s'))) } |]

-- | Patch the XMLFile with another XMLFile. Based on RFC 5261.
xmlFilePatch :: MonadIO m => Ptr XMLFile -> Ptr XMLFile -> m ()
xmlFilePatch ptr patch = liftIO $ [C.exp| void { $(XMLFile* ptr)->Patch($(XMLFile* patch)) } |]
