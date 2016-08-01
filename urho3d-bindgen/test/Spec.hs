import Test.Tasty
import Test.Tasty.HUnit

import Graphics.Urho3D.Interface.AST
import Graphics.Urho3D.Interface.AST.Type

testParse :: Parser a -> String -> a
testParse p s = case runCppParser (fullParser p) "" s of 
  Left er -> error (show er)
  Right a -> a 

main :: IO ()
main = defaultMain $ testGroup "Tests" [
    testGroup "Interface parser" [
      testCase "void" $ CppPodType CppVoid @=? testParse cppType "void"
    , testCase "void*" $ CppPointer (CppPodType CppVoid) @=? testParse cppType "void*"
    , testCase "void**" $ CppPointer (CppPointer (CppPodType CppVoid)) @=? testParse cppType "void**"
    , testCase "long double" $ CppPodType CppLongDouble @=? testParse cppType "long double"
    , testCase "long long" $ CppPodType (CppIntegral $ CppInt Nothing (Just LongLong)) @=? testParse cppType "long long"
    , testCase "unsigned long" $ CppPodType (CppIntegral $ CppInt (Just Unsigned) (Just Long)) @=? testParse cppType "unsigned long"
    , testCase "long unsigned" $ CppPodType (CppIntegral $ CppInt (Just Unsigned) (Just Long)) @=? testParse cppType "long unsigned"
    , testCase "long unsigned long" $ CppPodType (CppIntegral $ CppInt (Just Unsigned) (Just LongLong)) @=? testParse cppType "long unsigned long"
    , testCase "signed char" $ CppPodType (CppChar (Just Signed)) @=? testParse cppType "signed char"
    , testCase "char" $ CppPodType (CppChar Nothing) @=? testParse cppType "char"
    , testCase "Vector" $ CppUserType "Vector" @=? testParse cppType "Vector"
    ]
  ]
