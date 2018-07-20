{-
 Copyright (c) 2008-2015 the Urho3D project.

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
-}
module Mover where

import Data.IORef
import Foreign
import Control.Lens hiding (Context, element)

import Graphics.Urho3D
import Sample

-- | Custom logic component for moving the animated model and rotating at area edges.
type Mover = CustomLogicComponent

-- | Internal state of component
data MoverState = MoverState {
  moverSpeed :: Float -- ^ Forward movement speed.
, moverRotateSpeed :: Float -- ^ Rotation speed.
, moverBounds :: BoundingBox -- ^ Movement boundaries.
}

-- | Custom logic component setup that holds state and callbacks.
type MoverSetup = CustomLogicComponentSetup MoverState
-- | Type hash of our mover component
type MoverType = StringHash

-- | Register mover within Urho engine, resulting type is used for creation of
-- the component instances.
registerMover :: MonadIO m => Ptr Context -> m MoverType
registerMover cntx = registerCustomComponent cntx "Mover" moverState moverDef
  where
  moverState = MoverState {
      moverSpeed = 0
    , moverRotateSpeed = 0
    , moverBounds = 0
    }

-- | Update internal state of mover. Set motion parameters: forward movement speed, rotation speed, and movement boundaries.
setMoverParameters :: MonadIO m => Ptr Mover -> Float -> Float -> BoundingBox -> m ()
setMoverParameters ptr moveSpeed rotateSpeed bounds = liftIO $ do
  ref <- getCustomComponentState ptr
  writeIORef ref $ MoverState {
      moverSpeed = moveSpeed
    , moverRotateSpeed = rotateSpeed
    , moverBounds = bounds
    }

-- | Return forward movement speed.
moverGetMoveSpeed :: MonadIO m => Ptr Mover -> m Float
moverGetMoveSpeed ptr = liftIO $ do
  ref <- getCustomComponentState ptr
  moverSpeed <$> readIORef ref

-- | Return rotation speed.
moverGetRotationSpeed :: MonadIO m => Ptr Mover -> m Float
moverGetRotationSpeed ptr = liftIO $ do
  ref <- getCustomComponentState ptr
  moverRotateSpeed <$> readIORef ref

-- | Return movement boundaries.
moverGetBounds :: MonadIO m => Ptr Mover -> m BoundingBox
moverGetBounds ptr = liftIO $ do
  ref <- getCustomComponentState ptr
  moverBounds <$> readIORef ref

-- | Custom logic component for rotating a scene node.
moverDef :: MoverSetup
moverDef = defaultCustomLogicComponent {
    componentUpdate = Just $ \ref compNode t -> do
      -- Component state is passed via reference
      MoverState {..} <- readIORef ref

      nodeTranslate compNode (vec3Forward * vec3 (moverSpeed * t)) TSLocal

      -- If in risk of going outside the plane, rotate the model right
      pos <- nodeGetPosition compNode
      when ((pos ^. x < moverBounds ^. minVector ^. x) || (pos ^. x > moverBounds ^. maxVector ^. x) ||
            (pos ^. z < moverBounds ^. minVector ^. z) || (pos ^. z > moverBounds ^. maxVector ^. z)) $
        nodeYaw compNode (moverRotateSpeed * t) TSLocal

      -- Get the model's first (only) animation state and advance its time. Note the convenience accessor to other components
      -- in the same scene node
      (model :: Ptr AnimatedModel) <- fromJustTrace "AnimatedModel mover" <$> nodeGetComponent compNode False
      whenM ((> 0) <$> animatedModelGetNumAnimationStates model) $ do
        states <- animatedModelGetAnimationStates model
        animationStateAddTime (head states) t
  }
