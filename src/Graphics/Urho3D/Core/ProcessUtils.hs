{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.Core.ProcessUtils(
    platform
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String 

C.context (C.cppCtx)
C.include "<Urho3D/Core/ProcessUtils.h>"
C.using "namespace Urho3D"

-- | Returns current platform
platform :: String
platform = unsafePerformIO $ peekCString =<< [C.exp| const char* { GetPlatform().CString() } |]