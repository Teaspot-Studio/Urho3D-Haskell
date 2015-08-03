{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.UI.Sprite(
    Sprite 
  , spriteContext
  , SharedSprite
  , SharedSpritePtr 
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.UI.Internal.Sprite
import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Control.Monad.IO.Class
import Data.Monoid
import Foreign 

C.context (C.cppCtx <> sharedSpritePtrCntx <> spriteCntx <> contextCntx)
C.include "<Urho3D/UI/Sprite.h>"
C.using "namespace Urho3D"

spriteContext :: C.Context 
spriteContext = sharedSpritePtrCntx <> spriteCntx

newSprite :: Ptr Context -> IO (Ptr Sprite)
newSprite ptr = [C.exp| Sprite* { new Sprite( $(Context* ptr) ) } |]

deleteSprite :: Ptr Sprite -> IO ()
deleteSprite ptr = [C.exp| void { delete $(Sprite* ptr) } |]

instance Createable Sprite where 
  type CreationOptions Sprite = Ptr Context 

  newObject = liftIO . newSprite
  deleteObject = liftIO . deleteSprite

sharedPtr "Sprite"