{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.Internal.SharedObject where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C
import Graphics.Urho3D.Core.Internal.Object
import Graphics.Urho3D.Container.Ptr
import Data.Monoid 

C.context (C.cppCtx <> objectCntx <> sharedObjectPtrCntx)
C.include "<Urho3D/Core/Object.h>"
C.using "namespace Urho3D"

instance AbstractType Object

sharedPtr "Object"