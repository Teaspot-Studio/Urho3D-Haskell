module Graphics.Urho3D.Graphics.Internal.Texture(
    Texture
  , SharedTexture
  , WeakTexture
  , textureCntx
  , sharedTexturePtrCntx
  , weakTexturePtrCntx
  ) where

import Graphics.Urho3D.Container.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import qualified Data.Map as Map

data Texture

textureCntx :: C.Context
textureCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "Texture", [t| Texture |])
    ]
  }

sharedPtrImpl "Texture"
sharedWeakPtrImpl "Texture"
