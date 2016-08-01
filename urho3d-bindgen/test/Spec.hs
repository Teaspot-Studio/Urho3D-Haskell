import Test.Tasty
import Test.Tasty.HUnit

import Graphics.Urho3D.Interface.AST
import Graphics.Urho3D.Interface.AST.Type

testParse :: Parser a -> String -> a
testParse p s = case runCppParser p "" s of 
  Left er -> error (show er)
  Right a -> a 

main :: IO ()
main = defaultMain $ testGroup "Tests" [
    testGroup "Interface parser" [
      testCase "void" $ CppPodType CppVoid @=? testParse cppType "void"
    ]
  ]
