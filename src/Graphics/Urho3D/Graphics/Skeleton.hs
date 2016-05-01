{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Skeleton(
    Skeleton 
  , skeletonContext
  , SharedSkeleton
  , SharedSkeletonPtr 
  ) where 

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Core.Context 
import Graphics.Urho3D.Createable
import Graphics.Urho3D.Container.Ptr
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign

import Graphics.Urho3D.Graphics.Internal.Skeleton

import Graphics.Urho3D.Core.Object 
import Graphics.Urho3D.Graphics.Camera
import Graphics.Urho3D.Scene.Scene 
import Graphics.Urho3D.Parent 

C.context (C.cppCtx <> sharedSkeletonPtrCntx <> skeletonCntx <> contextContext)
C.include "<Urho3D/Graphics/Skeleton.h>"
C.using "namespace Urho3D"

skeletonContext :: C.Context 
skeletonContext = sharedSkeletonPtrCntx <> skeletonCntx

instance Createable (Ptr Skeleton) where 
  type CreationOptions (Ptr Skeleton) = ()

  newObject _ = liftIO $ [C.exp| Skeleton* { new Skeleton() } |]
  deleteObject ptr = liftIO $ [C.exp| void { delete $(Skeleton* ptr) } |]

sharedPtr "Skeleton"