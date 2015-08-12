{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urho3D.UI.Internal.LineEdit(
    LineEdit 
  , lineEditCntx
  , sharedLineEditPtrCntx
  , SharedLineEdit
  , SharedLineEditPtr(..)
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data LineEdit

lineEditCntx :: C.Context 
lineEditCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "LineEdit", [t| LineEdit |])
    ]
  } 

sharedPtrImpl "LineEdit"