{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Font(
    Font
  , fontContext
  , SharedFont
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Resource.Resource
import Graphics.Urho3D.UI.Internal.Font
import Data.Monoid
import Foreign

C.context (C.cppCtx <> fontCntx <> contextContext <> resourceContext <> sharedFontPtrCntx)
C.include "<Urho3D/UI/Font.h>"
C.using "namespace Urho3D"

fontContext :: C.Context
fontContext = fontCntx <> resourceContext <> sharedFontPtrCntx

instance Creatable (Ptr Font) where
  type CreationOptions (Ptr Font) = Ptr Context

  newObject ptr = liftIO $ [C.exp| Font* { new Font( $(Context* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Font* ptr) } |]

instance ResourceType Font where
  resourceType _ = StringHash . fromIntegral $ [C.pure| unsigned int { Font::GetTypeStatic().Value() } |]

sharedPtr "Font"
