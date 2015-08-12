{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.UI.Internal.Text(
    Text 
  , textCntx
  , sharedTextPtrCntx
  , SharedText
  , SharedTextPtr(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Text

textCntx :: C.Context 
textCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Text", [t| Text |])
    ]
  } 

sharedPtrImpl "Text"