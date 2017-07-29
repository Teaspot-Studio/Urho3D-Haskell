{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.ToolTip(
    ToolTip
  , toolTipContext
  , SharedToolTip
  , toolTipSetDelay
  , toolTipGetDelay
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.ToolTip
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Serializable
import Graphics.Urho3D.UI.Element

C.context (C.cppCtx <> sharedToolTipPtrCntx <> toolTipCntx <> contextContext <> uiElementContext <> animatableContext <> serializableContext <> objectContext)
C.include "<Urho3D/UI/ToolTip.h>"
C.using "namespace Urho3D"

toolTipContext :: C.Context
toolTipContext = sharedToolTipPtrCntx <> toolTipCntx

instance Creatable (Ptr ToolTip) where
  type CreationOptions (Ptr ToolTip) = Ptr Context

  newObject ptr = liftIO $ [C.exp| ToolTip* { new ToolTip( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(ToolTip* ptr) } |]

deriveParents [''Object, ''Serializable, ''Animatable, ''UIElement] ''ToolTip

instance UIElem ToolTip where
  uiElemType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { ToolTip::GetTypeStatic().Value() } |]

sharedPtr "ToolTip"

-- | Set the delay in seconds until the tooltip shows once hovering. Set zero to use the default from the UI subsystem.
-- void SetDelay(float delay);
toolTipSetDelay :: (Parent ToolTip a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'ToolTip' or ascentor
  -> Float
  -> m ()
toolTipSetDelay p v = liftIO $ do
  let ptr = parentPointer p
      v' = realToFrac v
  [C.exp| void { $(ToolTip* ptr)->SetDelay($(float v')) } |]

-- | Return the delay in seconds until the tooltip shows once hovering.
-- float GetDelay() const { return delay_; }
toolTipGetDelay :: (Parent ToolTip a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to 'ToolTip' or ascentor
  -> m Float
toolTipGetDelay p = liftIO $ do
  let ptr = parentPointer p
  realToFrac <$> [C.exp| float { $(ToolTip* ptr)->GetDelay() } |]
