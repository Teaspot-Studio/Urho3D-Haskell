{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.FontFaceFreeType(
    FontFaceFreeType
  , fontFaceFreeTypeContext
  , SharedFontFaceFreeType
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Core.Context
import Graphics.Urho3D.Creatable
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.UI.Font
import Graphics.Urho3D.UI.FontFace
import Graphics.Urho3D.UI.Internal.FontFaceFreeType

C.context (C.cppCtx
  <> fontFaceFreeTypeCntx
  <> contextContext
  <> sharedFontFaceFreeTypePtrCntx
  <> fontFaceContext
  <> fontContext
  )
C.include "<Urho3D/Container/Ptr.h>"
C.include "<Urho3D/Container/HashMap.h>"
C.include "<Urho3D/UI/FontFaceFreeType.h>"
C.using "namespace Urho3D"

deriveParents [''FontFace] ''FontFaceFreeType

fontFaceFreeTypeContext :: C.Context
fontFaceFreeTypeContext = fontFaceFreeTypeCntx <> sharedFontFaceFreeTypePtrCntx

instance Creatable (Ptr FontFaceFreeType) where
  type CreationOptions (Ptr FontFaceFreeType) = Ptr Font

  newObject ptr = liftIO $ [C.exp| FontFaceFreeType* { new FontFaceFreeType( $(Font* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(FontFaceFreeType* ptr) } |]

sharedPtr "FontFaceFreeType"
