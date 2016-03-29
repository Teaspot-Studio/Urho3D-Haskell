{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Graphics.StaticModel(
    StaticModel
  , staticModelContext
  , staticModelSetModel
  , staticModelSetMaterial
  , staticModelSetGeometryMaterial
  , staticModelSetOcclusionLodLevel
  , staticModelApplyMaterialList
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Graphics.Internal.StaticModel
import Graphics.Urho3D.Monad
import Data.Monoid
import Foreign
import Foreign.C.String 
import System.IO.Unsafe (unsafePerformIO) 

import Graphics.Urho3D.Graphics.Drawable 
import Graphics.Urho3D.Graphics.Model 
import Graphics.Urho3D.Graphics.Material
import Graphics.Urho3D.Math.StringHash 
import Graphics.Urho3D.Scene.Component
import Graphics.Urho3D.Scene.Node

C.context (C.cppCtx <> staticModelCntx <> componentContext <> stringHashContext <> drawableContext <> modelContext <> materialContext)
C.include "<Urho3D/Graphics/StaticModel.h>"
C.using "namespace Urho3D"

staticModelContext :: C.Context 
staticModelContext = staticModelCntx <> componentContext <> stringHashContext

instance Parent Component StaticModel where 
  castToParent ptr = [C.pure| Component* { (Component*)$(StaticModel* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| StaticModel* { (StaticModel*)$(Component* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance Parent Drawable StaticModel where 
  castToParent ptr = [C.pure| Drawable* { (Drawable*)$(StaticModel* ptr) } |]
  castToChild ptr = 
    let child = [C.pure| StaticModel* { (StaticModel*)$(Drawable* ptr) } |]
    in if child == nullPtr then Nothing else Just child

instance NodeComponent StaticModel where 
  nodeComponentType _ = unsafePerformIO $ [C.block| StringHash* {
    static StringHash h = StaticModel::GetTypeStatic();
    return &h;
  } |]

-- | Set model
staticModelSetModel :: (Parent StaticModel a, Pointer p a, Parent Model b, Pointer pm b, MonadIO m)
  => p -- ^ Pointer to static model 
  -> pm -- ^ Pointer to model 
  -> m ()
staticModelSetModel p pm = liftIO $ do 
  let ptr = parentPointer p 
      ptrm = parentPointer pm 
  [C.exp| void { $(StaticModel* ptr)->SetModel($(Model* ptrm)) } |]

-- | Set material on all geometries.
staticModelSetMaterial :: (Parent StaticModel a, Pointer p a, Parent Material b, Pointer pm b, MonadIO m)
  => p -- ^ Pointer to static model 
  -> pm -- ^ Pointer to material 
  -> m ()
staticModelSetMaterial p pm = liftIO $ do 
  let ptr = parentPointer p 
      ptrm = parentPointer pm 
  [C.exp| void { $(StaticModel* ptr)->SetMaterial($(Material* ptrm)) } |]

-- | Set material on one geometry. Return true if successful.
staticModelSetGeometryMaterial :: (Parent StaticModel a, Pointer p a, Parent Material b, Pointer pm b, MonadIO m)
  => p -- ^ Pointer to static model 
  -> Word -- ^ index
  -> pm -- ^ Pointer to material 
  -> m Bool
staticModelSetGeometryMaterial p i pm = liftIO $ do 
  let ptr = parentPointer p
      wi = fromIntegral i
      ptrm = parentPointer pm 
  toBool <$> [C.exp| int { $(StaticModel* ptr)->SetMaterial($(unsigned int wi), $(Material* ptrm)) } |]

-- | Set occlusion LOD level. By default (M_MAX_UNSIGNED) same as visible.
staticModelSetOcclusionLodLevel :: (Parent StaticModel a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to static model 
  -> Word -- ^ level
  -> m ()
staticModelSetOcclusionLodLevel p i = liftIO $ do 
  let ptr = parentPointer p 
      wi = fromIntegral i 
  [C.exp| void { $(StaticModel* ptr)->SetOcclusionLodLevel($(unsigned int wi)) } |]

-- | Apply default materials from a material list file. If filename is empty (default), the model's resource name with extension .txt will be used.
staticModelApplyMaterialList :: (Parent StaticModel a, Pointer p a, MonadIO m)
  => p -- ^ Pointer to static model
  -> Maybe String
  -> m ()
staticModelApplyMaterialList p Nothing = liftIO $ do 
  let ptr = parentPointer p 
  [C.exp| void { $(StaticModel* ptr)->ApplyMaterialList() } |]
staticModelApplyMaterialList p (Just fileName) = liftIO $ withCString fileName $ \fileName' -> do 
  let ptr = parentPointer p 
  [C.exp| void { $(StaticModel* ptr)->ApplyMaterialList(String($(const char* fileName'))) } |]