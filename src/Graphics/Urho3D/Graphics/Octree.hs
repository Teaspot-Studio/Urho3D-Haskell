{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.Octree(
    Octree
  , Octant
  , octreeContext
  , octreeSetSize
  , octreeUpdate
  , octreeAddManualDrawable
  , octreeRemoveManualDrawable
  , octreeGetDrawables
  , octreeRaycast
  , octreeRaycastSingle
  , octreeGetNumLevels
  , octreeQueueUpdate
  , octreeCancelUpdate
  , octreeDrawDebugGeometry
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Data.Monoid
import Foreign
import Graphics.Urho3D.Graphics.Internal.Octree
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Scene.Node

import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Graphics.Drawable
import Graphics.Urho3D.Graphics.Internal.OctreeQuery
import Graphics.Urho3D.Math.BoundingBox
import Graphics.Urho3D.Monad
import Graphics.Urho3D.Parent
import Graphics.Urho3D.Scene.Animatable
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Serializable

C.context (C.cppCtx
  <> octreeCntx
  <> componentContext
  <> stringHashContext
  <> animatableContext
  <> serializableContext
  <> objectContext
  <> boundingBoxContext
  <> octreeQueryCntx
  <> drawableContext
  )
C.include "<Urho3D/Graphics/Octree.h>"
C.using "namespace Urho3D"

octreeContext :: C.Context
octreeContext = octreeCntx
  <> componentContext
  <> stringHashContext

deriveParents [''Object, ''Serializable, ''Animatable, ''Component, ''Octant] ''Octree
deriveParents [''Object, ''Serializable, ''Animatable, ''Component] ''Octant

instance NodeComponent Octree where
  nodeComponentType _ = unsafePerformIO $ StringHash . fromIntegral <$> [C.exp|
    unsigned int { Octree::GetTypeStatic().Value() } |]

-- | Set size and maximum subdivision levels. If octree is not empty, drawable objects will be temporarily moved to the root.
-- void SetSize(const BoundingBox& box, unsigned numLevels);
octreeSetSize :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> BoundingBox
  -> Word -- ^ Num levels
  -> m ()
octreeSetSize p box levels = liftIO $ with box $ \box' -> do
  let ptr = parentPointer p
      levels' = fromIntegral levels
  [C.exp| void { $(Octree* ptr)->SetSize(*$(BoundingBox* box'), $(unsigned int levels')) } |]

-- | Update and reinsert drawable objects.
-- void Update(const FrameInfo& frame);
octreeUpdate :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> FrameInfo -- ^ frame
  -> m ()
octreeUpdate p frame = liftIO $ with frame $ \frame' -> do
  let ptr = parentPointer p
  [C.exp| void { $(Octree* ptr)->Update(*$(FrameInfo* frame')) } |]

-- | Add a drawable manually.
-- void AddManualDrawable(Drawable* drawable);
octreeAddManualDrawable :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> Ptr Drawable
  -> m ()
octreeAddManualDrawable p dr = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Octree* ptr)->AddManualDrawable($(Drawable* dr)) } |]

-- | Remove a manually added drawable.
-- void RemoveManualDrawable(Drawable* drawable);
octreeRemoveManualDrawable :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> Ptr Drawable
  -> m ()
octreeRemoveManualDrawable p dr = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Octree* ptr)->RemoveManualDrawable($(Drawable* dr)) } |]

-- | Return drawable objects by a query.
-- void GetDrawables(OctreeQuery& query) const;
octreeGetDrawables :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> Ptr OctreeQuery
  -> m ()
octreeGetDrawables p q = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Octree* ptr)->GetDrawables(*$(OctreeQuery* q)) } |]

-- | Return drawable objects by a ray query.
-- void Raycast(RayOctreeQuery& query) const;
octreeRaycast :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> Ptr RayOctreeQuery
  -> m ()
octreeRaycast p q = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Octree* ptr)->Raycast(*$(RayOctreeQuery* q)) } |]

-- | Return the closest drawable object by a ray query.
-- void RaycastSingle(RayOctreeQuery& query) const;
octreeRaycastSingle :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> Ptr RayOctreeQuery
  -> m ()
octreeRaycastSingle p q = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Octree* ptr)->RaycastSingle(*$(RayOctreeQuery* q)) } |]

-- | Return subdivision levels.
-- unsigned GetNumLevels() const { return numLevels_; }
octreeGetNumLevels :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> m Word
octreeGetNumLevels p = liftIO $ do
  let ptr = parentPointer p
  fromIntegral <$> [C.exp| unsigned int { $(Octree* ptr)->GetNumLevels() } |]

-- | Mark drawable object as requiring an update and a reinsertion.
-- void QueueUpdate(Drawable* drawable);
octreeQueueUpdate :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> Ptr Drawable
  -> m ()
octreeQueueUpdate p dr = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Octree* ptr)->QueueUpdate($(Drawable* dr)) } |]

-- | Cancel drawable object's update.
-- void CancelUpdate(Drawable* drawable);
octreeCancelUpdate :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> Ptr Drawable
  -> m ()
octreeCancelUpdate p dr = liftIO $ do
  let ptr = parentPointer p
  [C.exp| void { $(Octree* ptr)->CancelUpdate($(Drawable* dr)) } |]

-- | Visualize the component as debug geometry.
-- void DrawDebugGeometry(bool depthTest);
octreeDrawDebugGeometry :: (Parent Octree a, Pointer ptr a, MonadIO m)
  => ptr -- ^ Pointer to octree or ascentor
  -> Bool -- ^ depth test
  -> m ()
octreeDrawDebugGeometry p v = liftIO $ do
  let ptr = parentPointer p
      v' = fromBool v
  [C.exp| void { $(Octree* ptr)->DrawDebugGeometry($(int v') != 0) } |]
