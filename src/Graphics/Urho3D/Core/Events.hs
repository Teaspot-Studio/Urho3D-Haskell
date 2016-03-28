module Graphics.Urho3D.Core.Events(
  -- | Events
    EventBeginFrame(..)
  , EventUpdate(..)
  , EventPostUpdate(..)
  , EventRenderUpdate(..)
  , EventPostRenderUpdate(..)
  , EventEndFrame(..)
  -- | Lenses
  , HasFrameNumber(..)
  , HasTimeStep(..)
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Math.StringHash
import Graphics.Urho3D.Core.Object
import Graphics.Urho3D.Core.Variant
import Data.Monoid
import Data.Maybe 
import Control.Lens 

C.context (C.cppCtx <> stringHashContext)
C.include "<Urho3D/Core/CoreEvents.h>"
C.using "namespace Urho3D"

-- | Frame begin event.
data EventBeginFrame = EventBeginFrame {
    _eventBeginFrameFrameNumber :: Int
  , _eventBeginFrameTimeStep :: Float
  } deriving (Show)

makeFields ''EventBeginFrame

instance Event EventBeginFrame where 
  eventID _ = [C.pure| const StringHash* {&E_BEGINFRAME} |]
  loadEventData vmap = do 
    pfi <- variantMapGet' vmap [C.pure| const StringHash* {&BeginFrame::P_FRAMENUMBER} |]
    pt <- variantMapGet' vmap [C.pure| const StringHash* {&BeginFrame::P_TIMESTEP} |]
    return $ EventBeginFrame {
      _eventBeginFrameFrameNumber = fromMaybe 0 pfi
    , _eventBeginFrameTimeStep = fromMaybe 0 pt 
    }

-- | Application-wide logic update event.
data EventUpdate = EventUpdate {
    _eventUpdateTimeStep :: Float
  } deriving (Show)

makeFields ''EventUpdate

instance Event EventUpdate where 
  eventID _ = [C.pure| const StringHash* {&E_UPDATE} |]
  loadEventData vmap = do 
    pt <- variantMapGet' vmap [C.pure| const StringHash* {&Update::P_TIMESTEP} |]
    return $ EventUpdate {
      _eventUpdateTimeStep = fromMaybe 0 pt 
    }

-- | Application-wide logic post-update event.
data EventPostUpdate = EventPostUpdate {
    _eventPostUpdateTimeStep :: Float
  } deriving (Show)

makeFields ''EventPostUpdate

instance Event EventPostUpdate where 
  eventID _ = [C.pure| const StringHash* {&E_POSTUPDATE} |]
  loadEventData vmap = do 
    pt <- variantMapGet' vmap [C.pure| const StringHash* {&PostUpdate::P_TIMESTEP} |]
    return $ EventPostUpdate {
      _eventPostUpdateTimeStep = fromMaybe 0 pt 
    }

-- | Render update event.
data EventRenderUpdate = EventRenderUpdate {
    _eventRenderUpdateTimeStep :: Float
  } deriving (Show)

makeFields ''EventRenderUpdate

instance Event EventRenderUpdate where 
  eventID _ = [C.pure| const StringHash* {&E_RENDERUPDATE} |]
  loadEventData vmap = do 
    pt <- variantMapGet' vmap [C.pure| const StringHash* {&RenderUpdate::P_TIMESTEP} |]
    return $ EventRenderUpdate {
      _eventRenderUpdateTimeStep = fromMaybe 0 pt 
    }

-- | Post-render update event.
data EventPostRenderUpdate = EventPostRenderUpdate {
    _eventPostRenderUpdateTimeStep :: Float
  } deriving (Show)

makeFields ''EventPostRenderUpdate

instance Event EventPostRenderUpdate where 
  eventID _ = [C.pure| const StringHash* {&E_POSTRENDERUPDATE} |]
  loadEventData vmap = do 
    pt <- variantMapGet' vmap [C.pure| const StringHash* {&PostRenderUpdate::P_TIMESTEP} |]
    return $ EventPostRenderUpdate {
      _eventPostRenderUpdateTimeStep = fromMaybe 0 pt 
    }

-- | Frame end event.
data EventEndFrame = EventEndFrame 
  deriving (Show)

makeFields ''EventEndFrame

instance Event EventEndFrame where 
  eventID _ = [C.pure| const StringHash* {&E_ENDFRAME} |]
  loadEventData _ = return $ EventEndFrame 