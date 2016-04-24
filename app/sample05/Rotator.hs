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
module Rotator where

import Data.IORef 
import Foreign 

import Graphics.Urho3D

-- | Custom logic component for rotating a scene node.
type Rotator = CustomLogicComponent
-- | Custom logic component setup that holds state and callbacks.
--
-- At the case our state is rotation speed as Vector3.
type RotatorSetup = CustomLogicComponentSetup Vector3
-- | Type hash of our rotator component
type RotatorType = ForeignPtr StringHash

-- | Register rotator within Urho engine, resulting type is used for creation of 
-- the component instances.
registerRotator :: MonadIO m => Ptr Context -> m RotatorType
registerRotator cntx = registerCustomComponent cntx "Rotator" (Vector3 10 20 30) rotatorDef 

-- | Custom logic component for rotating a scene node.
rotatorDef :: RotatorSetup
rotatorDef = defaultCustomLogicComponent {
    componentUpdate = Just $ \ref node t -> do 
      -- Component state is passed via reference
      rotSpeed <- readIORef ref
      -- Components have their scene node as a member variable for convenient access. Rotate the scene node now: construct a
      -- rotation quaternion from Euler angles, scale rotation speed with the scene update time step
      let Vector3 x y z = rotSpeed
          q = quaternionFromEuler (x*t) (y*t) (z*t)
      nodeRotate node q TS'Local
  }