{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Cursor(
    Cursor
  , cursorContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Cursor
import Graphics.Urho3D.UI.BorderImage
import Graphics.Urho3D.UI.Element
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> cursorCntx <> borderImageContext <> uiElementContext)
C.include "<Urho3D/UI/Cursor.h>"
C.using "namespace Urho3D"

cursorContext :: C.Context 
cursorContext = cursorCntx <> borderImageContext <> uiElementContext

instance Parent UIElement Cursor where 
  castToParent ptr = [C.pure| UIElement* { (UIElement*)$(Cursor* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Cursor* { (Cursor*)$(UIElement* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Parent BorderImage Cursor where 
  castToParent ptr = [C.pure| BorderImage* { (BorderImage*)$(Cursor* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| Cursor* { (Cursor*)$(BorderImage* ptr) } |]
    in if child == nullPtr then Nothing else Just child