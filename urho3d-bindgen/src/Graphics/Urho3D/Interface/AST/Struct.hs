module Graphics.Urho3D.Interface.AST.Struct(
    CppStruct(..)
  , FieldName
  ) where

import Graphics.Urho3D.Interface.AST.Type 

type FieldName = String 

-- | C++ struct for generation of 'Storable' and other stuff
data CppStruct = CppStruct {
  cppStructFields :: [(CppType, FieldName)]
}

