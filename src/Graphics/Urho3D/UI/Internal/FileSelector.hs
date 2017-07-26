module Graphics.Urho3D.UI.Internal.FileSelector(
    FileSelector
  , fileSelectorCntx
  , sharedFileSelectorPtrCntx
  , weakFileSelectorPtrCntx
  , SharedFileSelector
  , WeakFileSelector
  , FileSelectorEntry(..)
  , HasName(..)
  , HasDirectory(..)
  ) where

import Control.Lens
import GHC.Generics
import Graphics.Urho3D.Container.Ptr
import qualified Data.Map as Map
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

-- resolve lens fields
import Graphics.Urho3D.Graphics.Internal.Skeleton

-- | File selector dialog
data FileSelector

-- | File selector's list entry (file or directory.)
data FileSelectorEntry = FileSelectorEntry {
  _fileSelectorEntryName :: !String -- ^ Name
, _fileSelectorEntryDirectory :: !Bool -- ^ Directory flag
} deriving (Generic)

makeFields ''FileSelectorEntry

fileSelectorCntx :: C.Context
fileSelectorCntx = mempty {
    C.ctxTypesTable = Map.fromList [
      (C.TypeName "FileSelector", [t| FileSelector |])
    , (C.TypeName "FileSelectorEntry", [t| FileSelectorEntry |])
    ]
  }

sharedPtrImpl "FileSelector"
sharedWeakPtrImpl "FileSelector"
