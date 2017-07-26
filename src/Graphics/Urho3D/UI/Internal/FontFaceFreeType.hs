module Graphics.Urho3D.UI.Internal.FontFaceFreeType(
    FontFaceFreeType
  , fontFaceFreeTypeCntx
  , sharedFontFaceFreeTypePtrCntx
  , SharedFontFaceFreeType
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data FontFaceFreeType

fontFaceFreeTypeCntx :: C.Context 
fontFaceFreeTypeCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "FontFaceFreeType", [t| FontFaceFreeType |])
    ]
  }

sharedPtrImpl "FontFaceFreeType"
