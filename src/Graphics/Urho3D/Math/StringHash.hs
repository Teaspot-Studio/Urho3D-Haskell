{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.StringHash(
    StringHash(..)
  , stringHashContext
  -- | Low-level
  , deleteStringHash
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C
import Text.RawString.QQ

import Graphics.Urho3D.Math.Internal.StringHash
import Graphics.Urho3D.Creatable
import Control.Monad.IO.Class
import Data.Monoid
import Foreign 
import Foreign.C.String 

C.context (C.cppCtx <> stringHashCntx)
C.include "<Urho3D/Math/StringHash.h>"
C.using "namespace Urho3D"

stringHashContext :: C.Context 
stringHashContext = stringHashCntx

newStringHash :: String -> IO (Ptr StringHash)
newStringHash str = withCString str $ \cstr -> do
  [C.exp| StringHash* { new StringHash( $(const char* cstr) ) } |]

deleteStringHash :: Ptr StringHash -> IO ()
deleteStringHash ptr = [C.exp| void { delete $(StringHash* ptr) } |]

instance Creatable (Ptr StringHash) where 
  type CreationOptions (Ptr StringHash) = String

  newObject = liftIO . newStringHash
  deleteObject = liftIO . deleteStringHash

C.verbatim [r|
template <class T>
class Traits
{
public:
    struct AlignmentFinder
    {
      char a; 
      T b;
    };

    enum {AlignmentOf = sizeof(AlignmentFinder) - sizeof(T)};
};
|]

instance Storable StringHash where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(StringHash) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<StringHash>::AlignmentOf } |]
  peek ptr = do 
    val <- fromIntegral <$> [C.exp| unsigned int { $(StringHash* ptr)->Value() } |]
    return $ StringHash val
  poke ptr (StringHash val) = do
    let val' = fromIntegral val
    [C.block| void { 
      *$(StringHash* ptr) = StringHash($(unsigned int val'));
    } |]