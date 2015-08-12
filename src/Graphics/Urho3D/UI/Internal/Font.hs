{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.UI.Internal.Font(
    Font
  , fontCntx
  , sharedFontPtrCntx
  , SharedFont 
  , SharedFontPtr(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Font

fontCntx :: C.Context 
fontCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Font", [t| Font |])
    ]
  }

sharedPtrImpl "Font"