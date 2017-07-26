module Graphics.Urho3D.UI.Internal.FontFaceBitmap(
    FontFaceBitmap
  , fontFaceBitmapCntx
  , sharedFontFaceBitmapPtrCntx
  , SharedFontFaceBitmap
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data FontFaceBitmap

fontFaceBitmapCntx :: C.Context 
fontFaceBitmapCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "FontFaceBitmap", [t| FontFaceBitmap |])
    ]
  }

sharedPtrImpl "FontFaceBitmap"
