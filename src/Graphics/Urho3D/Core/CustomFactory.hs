{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Core.CustomFactory(
    CustomFactory
  , SharedCustomFactory
  , customFactoryContext
  , newCustomFactory
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Internal.CustomFactory
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.ObjectFactory
import Graphics.Urho3D.Core.TypeInfo
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Container.Str
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Text.RawString.QQ
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> C.funConstCtx <> objectContext <> customFactoryCntx <> contextContext <> stringHashContext <> sharedCustomFactoryPtrCntx <> typeInfoContext <> stringContext <> objectFactoryContext)
C.include "<Urho3D/Core/Object.h>"
C.using "namespace Urho3D"

customFactoryContext :: C.Context 
customFactoryContext = objectContext <> stringHashContext <> customFactoryCntx <> sharedCustomFactoryPtrCntx <> objectFactoryContext

-- | Haskell constructor of Urho Object
type ObjectMaker = Ptr Context -> IO (Ptr Object)

C.verbatim [r|

extern "C" typedef Object* (*haskellObjFunc)(Context*);

class CustomFactory : public ObjectFactory {
public:
  /// Construct
  CustomFactory(Context* context, TypeInfo* tinfo, haskellObjFunc createFunc)
    : ObjectFactory(context)
    , createFunc_(createFunc)
  {
    assert(tinfo);
    assert(createFunc_);
    typeInfo_ = tinfo;
  }

  /// Create an object of the specific type.
  virtual SharedPtr<Object> CreateObject() {
    return SharedPtr<Object>(createFunc_(context_));
  }

private:
  haskellObjFunc createFunc_ = NULL;
};
|]

-- | Make factory from type info and Haskell function
newCustomFactory :: Ptr Context -> Ptr TypeInfo -> ObjectMaker -> IO (Ptr CustomFactory)
newCustomFactory ptr pinfo maker = [C.exp| CustomFactory* {
    new CustomFactory($(Context* ptr), $(TypeInfo* pinfo), $funConst:(Object* (*maker)(Context*)))
  } |]

deleteCustomFactory :: Ptr CustomFactory -> IO ()
deleteCustomFactory ptr = [C.exp| void {delete $(CustomFactory* ptr)} |]

instance Createable (Ptr CustomFactory) where 
  type CreationOptions (Ptr CustomFactory) = (Ptr Context, Ptr TypeInfo, ObjectMaker)

  newObject (ptr, pinfo, maker) = liftIO $ newCustomFactory ptr pinfo maker
  deleteObject = liftIO . deleteCustomFactory

deriveParent ''ObjectFactory ''CustomFactory
sharedPtr "CustomFactory"