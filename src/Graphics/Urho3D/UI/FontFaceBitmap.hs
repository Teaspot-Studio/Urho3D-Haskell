{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Urho3D.UI.FontFaceBitmap(
    FontFaceBitmap
  , fontFaceBitmapContext
  , SharedFontFaceBitmap
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
import Graphics.Urho3D.UI.Internal.FontFaceBitmap

C.context (C.cppCtx
  <> fontFaceBitmapCntx
  <> contextContext
  <> sharedFontFaceBitmapPtrCntx
  <> fontContext
  <> fontFaceContext
  )
C.include "<Urho3D/Container/Ptr.h>"
C.include "<Urho3D/Container/HashMap.h>"
C.include "<Urho3D/UI/FontFaceBitmap.h>"
C.using "namespace Urho3D"

deriveParents [''FontFace] ''FontFaceBitmap

fontFaceBitmapContext :: C.Context
fontFaceBitmapContext = fontFaceBitmapCntx <> sharedFontFaceBitmapPtrCntx

instance Creatable (Ptr FontFaceBitmap) where
  type CreationOptions (Ptr FontFaceBitmap) = Ptr Font

  newObject ptr = liftIO $ [C.exp| FontFaceBitmap* { new FontFaceBitmap( $(Font* ptr) ) } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(FontFaceBitmap* ptr) } |]

sharedPtr "FontFaceBitmap"
