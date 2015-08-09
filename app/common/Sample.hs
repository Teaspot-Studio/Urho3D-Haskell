{-# LANGUAGE TypeFamilies, RecordWildCards, ScopedTypeVariables #-}
module Sample(
    Sample
  , newSample
  , deleteSample
  , sampleStart 
  , sampleStop 
  ) where 

import Graphics.Urho3D
import Data.Word 
import Foreign
import Data.StateVar
import Data.Maybe 
import Control.Monad.State.Strict
import Control.Lens hiding (Context)

import Internal.Sample

newSample :: Ptr Context 
  -> String -- ^ Joystick patch string
  -> IO Sample 
newSample context joystickPatch = do 
  app <- newObject context 
  sprite <- newObject nullPtr 
  scene <- newObject nullPtr
  camNode <- newObject nullPtr
  return $ Sample {
    _sampleApplication = app 
  , _sampleYaw = 0
  , _samplePitch = 0
  , _sampleTouchEnabled = False
  , _sampleScreenSettingsIndex = maxBound
  , _sampleScreenJoystickIndex = maxBound 
  , _samplePaused = False 
  , _sampleLogo = sprite
  , _sampleScene = scene
  , _sampleCameraNode = camNode
  , _sampleJoystickPatch = joystickPatch
  }

deleteSample :: Sample -> IO ()
deleteSample s = do
  deleteObject $ s ^. sampleApplication
  deleteObject $ s ^. sampleLogo
  deleteObject $ s ^. sampleScene
  deleteObject $ s ^. sampleCameraNode

sampleSetup :: StateT Sample IO ()
sampleSetup = do
  sName <- use sampleName
  app <- use sampleApplication

  startupParameter app "WindowTitle" $= sName

  fs <- fromJust <$> getSubsystem app
  prefDir <- getAppPreferencesDir fs "urho3d" "logs"
  startupParameter app "LogName" $= prefDir ++ sName ++ ".log"

  startupParameter app "FullScreen" $= False 
  startupParameter app "Headless" $= False 

sampleStart :: StateT Sample IO ()
sampleStart = do 
  app <- use sampleApplication
  if platform == "Android" || platform == "iOS" 
  then initTouchInput
  else do 
    is <- fromJust <$> getSubsystem app
    jcount <- getNumJoysticks is
    when (jcount == 0) $ subscribeToEvent app EventTouchBegin (const handleTouchBegin)

  createLogo
  setWindowTitleAndIcon
  createConsoleAndDebugHud

  subscribeToEvent app EventKeyDown handleKeyDown 
  subscribeToEvent app EventSceneUpdate handleSceneUpdate

initTouchInput :: StateT Sample IO ()
initTouchInput = do 
  app <- use sampleApplication
  sampleTouchEnabled .= True 

  resCache <- fromJust <$> getSubsystem app
  input <- fromJust <$> getSubsystem app 
  layoutM <- cacheGetResource resCache "UI/ScreenJoystick_Samples.xml" True 
  case layoutM of 
    Nothing -> error "Cannot open UI/ScreenJoystick_Samples.xml"
    Just layout -> do 
      joystickPatch <- use sampleJoystickPatch
      unless (null joystickPatch) $ do 
        cntx <- getContext app 
        withObject cntx $ \patchFile -> do 
          whenM (xmlFileFromString patchFile joystickPatch) $ xmlFilePatch layout patchFile

      defStyleM <- cacheGetResource resCache "UI/DefaultStyle.xml" True
      _ <- whenJust defStyleM $ \defStyle -> do
        joystick <- addScreenJoystick input layout defStyle
        setScreenJoystickVisible input joystick True
      return ()

setLogoVisible :: Bool -> StateT Sample IO ()
setLogoVisible flag = do 
  ptr <- use sampleLogo 
  unless (isNull ptr) $ uiElementSetVisible (parentPointer ptr) flag

sampleStop :: StateT Sample IO ()
sampleStop = do 
  eng <- applicationEngine =<< use sampleApplication
  engineDumpResources eng True

createLogo :: StateT Sample IO ()
createLogo = do 
  app <- use sampleApplication
  cache <- fromJust <$> getSubsystem app 

  logoTextureM <- cacheGetResource cache "Textures/LogoLarge.png" True
  case logoTextureM of 
    Nothing -> return ()
    Just (logoTexture :: Ptr Texture2D) -> do 
      ui <- fromJust <$> getSubsystem app
      sprite <- newObject =<< createChildSimple =<< uiRoot ui 
      sampleLogo .= sprite

      spriteSetTexture sprite $ parentPointer logoTexture
      twidth <- textureWidth $ parentPointer logoTexture
      theight <- textureHeight $ parentPointer logoTexture
      spriteSetScale sprite $ 256 / fromIntegral twidth
      uiElementSetSize (parentPointer sprite) twidth theight
      spriteSetHotSpot sprite 0 theight
      uiElementSetAlignment (parentPointer sprite) AlignmentLeft AlignmentBottom 
      uiElementSetOpacity (parentPointer sprite) 0.75
      uiElementSetPriority (parentPointer sprite) (-100)

setWindowTitleAndIcon :: StateT Sample IO ()
setWindowTitleAndIcon = do 
  app <- use sampleApplication
  cache <- fromJust <$> getSubsystem app 
  graphics <- fromJust <$> getSubsystem app 
  iconM <- cacheGetResource cache "Textures/UrhoIcon.png" True 
  _ <- whenJust iconM $ graphicsSetWindowIcon graphics
  graphicsSetWindowTitle graphics "Urho3D Sample"

createConsoleAndDebugHud :: StateT Sample IO ()
createConsoleAndDebugHud = undefined

handleKeyDown :: EventData EventKeyDown -> IO ()
handleKeyDown = undefined

handleSceneUpdate :: EventData EventSceneUpdate -> IO ()
handleSceneUpdate = undefined

handleTouchBegin :: IO ()
handleTouchBegin = undefined