module Graphics.Urho3D.Core.ProcessUtils(
    platform
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C
import Graphics.Urho3D.Container.Str
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid

C.context (C.cppCtx <> stringContext)
C.include "<Urho3D/Core/ProcessUtils.h>"
C.using "namespace Urho3D"

-- | Returns current platform
platform :: String
platform = unsafePerformIO $ loadUrhoString =<< [C.exp| String* { new String(GetPlatform()) } |]