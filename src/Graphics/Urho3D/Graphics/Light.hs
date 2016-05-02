{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Light(
    Light
  , PODVectorLightPtr
  , LightType(..)
  , lightContext
  , lightSetLightType
  , lightSetRange
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.Light
import Graphics.Urho3D.Monad
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Scene.Node

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Container.Vector
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Graphics.Internal.Drawable 
import Graphics.Urho3D.Scene.Component 
import Graphics.Urho3D.Parent

C.context (C.cppCtx <> lightCntx <> componentContext <> stringHashContext <> drawableCntx <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/Graphics/Light.h>"
C.using "namespace Urho3D"

podVectorPtr "Light"

lightContext :: C.Context 
lightContext = lightCntx <> componentContext <> stringHashContext

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Drawable] ''Light

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

-- | Set range.
lightSetRange :: (Parent Light a, Pointer p a, MonadIO m) 
  => p -- ^ Pointer to light 
  -> Float -- ^ Light max range
  -> m ()
lightSetRange p v = liftIO $ do 
  let ptr = parentPointer p 
      v' = realToFrac v
  [C.exp| void { $(Light* ptr)->SetRange($(float v')) } |]
