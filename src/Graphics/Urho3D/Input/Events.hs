{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Urho3D.Input.Events(
    EventTouchBegin(..)
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Data.Monoid
import Data.Maybe 

C.context (C.cppCtx <> stringHashContext)
C.include "<Urho3D/Input/InputEvents.h>"
C.using "namespace Urho3D"

-- | Fires when user touches the screen
data EventTouchBegin = EventTouchBegin

instance Event EventTouchBegin where 
  data EventData EventTouchBegin = EventTouchBeginData {
    touchId :: Int 
  , touchX :: Int 
  , touchY :: Int 
  , touchPressure :: Float
  }

  eventID _ = [C.pure| const StringHash* {&E_TOUCHBEGIN} |]
  loadEventData vmap = do 
    tid <- variantMapGet' vmap [C.pure| const StringHash* {&TouchBegin::P_TOUCHID} |] 
    tx <- variantMapGet' vmap [C.pure| const StringHash* {&TouchBegin::P_X} |] 
    ty <- variantMapGet' vmap [C.pure| const StringHash* {&TouchBegin::P_Y} |]
    tp <- variantMapGet' vmap [C.pure| const StringHash* {&TouchBegin::P_PRESSURE} |] 
    return $ EventTouchBeginData {
      touchId = fromMaybe 0 tid 
    , touchX = fromMaybe 0 tx
    , touchY = fromMaybe 0 ty 
    , touchPressure = fromMaybe 0 tp
    }