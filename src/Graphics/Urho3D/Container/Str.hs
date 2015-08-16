{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Container.Str(
    UrhoString
  , UrhoWString
  , stringContext
  , loadUrhoString
  , loadUrhoText
  , loadConstUrhoString
  , loadConstUrhoText
  ) where 

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.Text as T 

import Graphics.Urho3D.Container.Internal.Str
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 
import Foreign.C.String 

C.context (C.cppCtx <> stringCntx)
C.include "<Urho3D/Container/Str.h>"
C.using "namespace Urho3D"

stringContext :: C.Context 
stringContext = stringCntx

instance Createable (Ptr UrhoString) where 
  type CreationOptions (Ptr UrhoString) = Either String T.Text

  newObject (Left s) = liftIO $ withCString s $ \s' -> [C.exp| String* { new String($(const char* s')) } |]
  newObject (Right s) = liftIO $ textAsPtrW32 s $ \s' -> [C.exp| String* { new String($(const wchar_t* s')) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(String* ptr) } |]

instance Createable (Ptr UrhoWString) where 
  type CreationOptions (Ptr UrhoWString) = T.Text

  newObject s = liftIO $ textAsPtrW32 s $ \s' -> [C.exp| WString* { new WString($(const wchar_t* s')) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(WString* ptr) } |]

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
  loadConstUrhoText

-- | Loads given Urho3D::String into Haskell Text WITHOUT deletion of source Urho string
loadConstUrhoText :: MonadIO m => Ptr UrhoWString -> m T.Text  
loadConstUrhoText wstr = liftIO $ textFromPtrW32 =<< [C.exp| const wchar_t* { $(WString* wstr)->CString() } |]