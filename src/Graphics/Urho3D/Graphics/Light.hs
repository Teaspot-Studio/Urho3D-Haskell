{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Light(
    Light
  , LightType(..)
  , lightContext
  , lightSetLightType
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Light
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Graphics.Drawable 
import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Node

C.context (C.cppCtx <> lightCntx <> componentContext <> stringHashContext <> drawableContext)
C.include "<Urho3D/Graphics/Light.h>"
C.using "namespace Urho3D"

lightContext :: C.Context 
lightContext = lightCntx <> componentContext <> stringHashContext

instance Parent Component Light where 
  castToParent ptr = [C.pure| Component* { (Component*)$(Light* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Light* { (Light*)$(Component* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Parent Drawable Light where 
  castToParent ptr = [C.pure| Drawable* { (Drawable*)$(Light* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Light* { (Light*)$(Drawable* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance NodeComponent Light where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = Light::GetTypeStatic();
    return &h;
  } |]

-- | Light types
data LightType = 
    LT'Directional
  | LT'Spot
  | LT'Point
  deriving (Eq, Show, Read, Enum, Bounded)

-- | Set light type
lightSetLightType :: (Parent Light a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to light 
  -> LightType -- ^ Type of light 
  -> m ()
lightSetLightType p lt = liftIO $ do 
  let ptr = parentPointer p 
      i = fromIntegral $ fromEnum lt 
  [C.exp| void { $(Light* ptr)->SetLightType((LightType)$(int i)) } |]
