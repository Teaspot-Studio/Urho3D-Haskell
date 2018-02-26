{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Container.Str(
    UrhoString
  , UrhoWString
  , StringVector
  , WStringVector
  , stringContext
  , loadUrhoString
  , loadUrhoText
  , loadConstUrhoString
  , loadConstUrhoText
  , loadConstUrhoWText
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.Text as T

import Graphics.Urho3D.Container.Internal.Str
import Graphics.Urho3D.Container.ForeignVector
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import Foreign.C.String

C.context (C.cppCtx <> stringCntx)
C.include "<Urho3D/Container/Str.h>"
C.using "namespace Urho3D"

C.verbatim "typedef Vector<String> StringVector;"
C.verbatim "typedef Vector<WString> WStringVector;"

stringContext :: C.Context
stringContext = stringCntx

instance Creatable (Ptr UrhoString) where
  type CreationOptions (Ptr UrhoString) = Either String T.Text

  newObject (Left s) = liftIO $ withCString s $ \s' -> [C.exp| String* { new String($(const char* s')) } |]
  newObject (Right s) = liftIO $ textAsPtrW32 s $ \s' -> [C.exp| String* { new String($(const wchar_t* s')) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(String* ptr) } |]

instance Creatable (Ptr UrhoWString) where
  type CreationOptions (Ptr UrhoWString) = T.Text

  newObject s = liftIO $ textAsPtrW32 s $ \s' -> [C.exp| WString* { new WString(String($(const wchar_t* s'))) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(WString* ptr) } |]

instance Creatable (Ptr StringVector) where
  type CreationOptions (Ptr StringVector) = ()

  newObject _ = liftIO [C.exp| StringVector* { new StringVector() } |]
  deleteObject ptr = liftIO $ [C.exp| void {delete $(StringVector* ptr)} |]

instance ReadableVector StringVector where
  type ReadVecElem StringVector = String
  foreignVectorLength ptr = fromIntegral <$>
    liftIO [C.exp| unsigned int {$(StringVector* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    peekCString =<< [C.exp| const char* { (*$(StringVector* ptr))[$(int i')].CString() } |]

instance WriteableVector StringVector where
  type WriteVecElem StringVector = String
  foreignVectorAppend ptr s = liftIO $ withCString s $ \s' ->
    [C.exp| void { $(StringVector* ptr)->Push(String($(const char* s'))) } |]

instance Creatable (Ptr WStringVector) where
  type CreationOptions (Ptr WStringVector) = ()

  newObject _ = liftIO [C.exp| WStringVector* { new WStringVector() } |]
  deleteObject ptr = liftIO $ [C.exp| void {delete $(WStringVector* ptr)} |]

instance ReadableVector WStringVector where
  type ReadVecElem WStringVector = T.Text
  foreignVectorLength ptr = fromIntegral <$>
    liftIO [C.exp| unsigned int {$(WStringVector* ptr)->Size()} |]
  foreignVectorElement ptr i = liftIO $ do
    let i' = fromIntegral i
    loadConstUrhoWText =<< [C.exp| WString* { &(*$(WStringVector* ptr))[$(int i')] } |]

instance WriteableVector WStringVector where
  type WriteVecElem WStringVector = T.Text
  foreignVectorAppend ptr s = liftIO $ textAsPtrW32 s $ \s' ->
    [C.exp| void { $(WStringVector* ptr)->Push(WString(String($(const wchar_t* s')))) } |]

-- | Loads given Urho3D::String into Haskell Stirng AND deletes the Urho string after creation
loadUrhoString :: (MonadMask m, MonadIO m) => Ptr UrhoString -> m String
loadUrhoString ptr = bracket (return ptr) deleteObject loadConstUrhoString

-- | Loads given Urho3D::String into Haskell Stirng WITHOUT deletion of source Urho string
loadConstUrhoString :: MonadIO m => Ptr UrhoString -> m String
loadConstUrhoString ptr = liftIO $ peekCString =<< [C.exp| const char* { $(String* ptr)->CString() } |]

-- | Loads given Urho3D::String into Haskell Text AND deletes the Urho string after creation
loadUrhoText :: (MonadMask m, MonadIO m) => Ptr UrhoString -> m T.Text
loadUrhoText ptr = bracket
  (liftIO [C.exp| WString* { new WString(*$(String* ptr)) } |])
  (\wstr -> deleteObject wstr >> deleteObject ptr)
  loadConstUrhoWText

-- | Loads given Urho3D::String into Haskell Text WITHOUT deletion of the Urho string after creation
loadConstUrhoText :: (MonadMask m, MonadIO m) => Ptr UrhoString -> m T.Text
loadConstUrhoText ptr = bracket
  (liftIO [C.exp| WString* { new WString(*$(String* ptr)) } |])
  deleteObject
  loadConstUrhoWText

-- | Loads given Urho3D::String into Haskell Text WITHOUT deletion of source Urho string
loadConstUrhoWText :: MonadIO m => Ptr UrhoWString -> m T.Text
loadConstUrhoWText wstr = liftIO $ textFromPtrW32 =<< [C.exp| const wchar_t* { $(WString* wstr)->CString() } |]
