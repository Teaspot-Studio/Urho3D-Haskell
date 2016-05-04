module Graphics.Urho3D.UI.Internal.Sprite(
    Sprite 
  , spriteCntx
  , sharedSpritePtrCntx
  , SharedSprite
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map

data Sprite

spriteCntx :: C.Context 
spriteCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Sprite", [t| Sprite |])
    ]
  } 

sharedPtrImpl "Sprite"