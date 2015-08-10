{-# LANGUAGE TypeFamilies, RecordWildCards, ScopedTypeVariables, MultiWayIf #-}
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
import Data.IORef 
import Data.Thyme
import Data.Proxy
import Control.Monad as Monad 
import Control.Monad.State.Strict as State
import Control.Lens hiding (Context)
import System.Locale 

import Internal.Sample

touchSensitivity :: Float 
touchSensitivity = 2.0

newSample :: Ptr Context 
  -> String -- ^ Sample name
  -> String -- ^ Joystick patch string
  -> IO Sample 
newSample context name joystickPatch = do 
  app <- newObject context 
  sprite <- newObject nullPtr 
  scene <- newObject nullPtr
  camNode <- newObject nullPtr
  pausedRef <- newIORef False
  settRef <- newIORef maxBound
  yawRef <- newIORef 0
  pitchRef <- newIORef 0
  touchEnabledRef <- newIORef False
  return $ Sample {
    _sampleApplication = app 
  , _sampleName = name
  , _sampleYaw = yawRef
  , _samplePitch = pitchRef
  , _sampleTouchEnabled = touchEnabledRef
  , _sampleScreenSettingsIndex = settRef
  , _sampleScreenJoystickIndex = maxBound 
  , _samplePaused = pausedRef 
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
  sample <- State.get

  if platform == "Android" || platform == "iOS" 
  then initTouchInput
  else do 
    is <- fromJust <$> getSubsystem app
    jcount <- getNumJoysticks is
    when (jcount == 0) $ subscribeToEvent app $ handleTouchBegin sample

  createLogo
  setWindowTitleAndIcon
  createConsoleAndDebugHud

  subscribeToEvent app $ handleKeyDown sample
  subscribeToEvent app $ handleSceneUpdate sample

initTouchInput :: StateT Sample IO ()
initTouchInput = do 
  app <- use sampleApplication

  touchRef <- use sampleTouchEnabled
  liftIO $ writeIORef touchRef True 

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
  unless (isNull ptr) $ uiElementSetVisible ptr flag

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
      (ui :: Ptr UI) <- fromJust <$> getSubsystem app
      sprite <- newObject =<< createChildSimple =<< uiRoot ui 
      sampleLogo .= sprite

      spriteSetTexture sprite logoTexture
      twidth <- textureWidth logoTexture
      theight <- textureHeight logoTexture
      spriteSetScale sprite $ 256 / fromIntegral twidth
      uiElementSetSize sprite twidth theight
      spriteSetHotSpot sprite 0 theight
      uiElementSetAlignment sprite AlignmentLeft AlignmentBottom 
      uiElementSetOpacity sprite 0.75
      uiElementSetPriority sprite (-100)

setWindowTitleAndIcon :: StateT Sample IO ()
setWindowTitleAndIcon = do 
  app <- use sampleApplication
  (cache :: Ptr ResourceCache) <- fromJust <$> getSubsystem app 
  (graphics :: Ptr Graphics) <- fromJust <$> getSubsystem app 
  iconM <- cacheGetResource cache "Textures/UrhoIcon.png" True 
  _ <- whenJust iconM $ graphicsSetWindowIcon graphics
  graphicsSetWindowTitle graphics "Urho3D Sample"

createConsoleAndDebugHud :: StateT Sample IO ()
createConsoleAndDebugHud = do 
  app <- use sampleApplication
  engine <- applicationEngine app 

  cache <- fromJust <$> getSubsystem app 
  xmlFileM <- cacheGetResource cache "UI/DefaultStyle.xml" True 
  _ <- whenJust xmlFileM $ \xmlFile -> do 
    consoleM <- engineCreateConsole engine
    whenJust consoleM $ \console -> do 
      consoleSetDefaultStyle console xmlFile 
      bg <- consoleGetBackground console 
      uiElementSetOpacity bg 0.8 

    debugHud <- engineCreateDebugHud engine 
    debugHudSetDefaultStyle debugHud xmlFile 

  return ()

handleKeyDown :: Sample -> EventKeyDown -> IO ()
handleKeyDown s (EventKeyDown{..}) = do 
  let app = s ^. sampleApplication
      key = pressKey
  engine <- applicationEngine app 
  (console :: Ptr Console) <- fromJust <$> getSubsystem app 
  (debugHud :: Ptr DebugHud) <- fromJust <$> getSubsystem app 
  (ui :: Ptr UI) <- fromJust <$> getSubsystem app 
  focusElem <- uiFocusElement ui 
  (input :: Ptr Input) <- fromJust <$> getSubsystem app 
  (cache :: Ptr ResourceCache) <- fromJust <$> getSubsystem app 
  (fs :: Ptr FileSystem) <- fromJust <$> getSubsystem app 

  if| key == KeyEsc -> do 
      -- Close console (if open) or exit when ESC is pressed    
      vis <- consoleIsVisible console 
      if vis  
        then consoleSetVisible console False 
        else engineExit engine 
    | key == KeyF1 -> consoleToggle console  -- Toggle console with F1
    | key == KeyF2 -> debugHudToggle debugHud -- Toggle debug HUD with F2
    | isNothing focusElem -> do -- Common rendering quality controls, only when UI has no focused element
      (renderer :: Ptr Renderer) <- fromJust <$> getSubsystem app

      -- Preferences / Pause 
      touchEnabled <- readIORef (s ^. sampleTouchEnabled)
      if | key == KeySelect && touchEnabled -> do 
          modifyIORef' (s ^. samplePaused) not 

          settIndex <- readIORef $ s ^. sampleScreenSettingsIndex
          if | settIndex == maxBound -> do 
                -- Lazy initialization
                layoutM <- cacheGetResource cache "UI/ScreenJoystickSettings_Samples.xml" True 
                styleM <- cacheGetResource cache "UI/DefaultStyle.xml" True 
                _ <- whenJust (liftM2 (,) layoutM styleM) $ \(layout, style) -> do
                  ji <- addScreenJoystick input layout style
                  writeIORef (s ^. sampleScreenSettingsIndex) ji
                return ()
             | otherwise -> setScreenJoystickVisible input settIndex =<< readIORef (s ^. samplePaused)
         | key == Key1 -> do -- Texture quality
          quality <- rendererGetTextureQuality renderer
          rendererSetTextureQuality renderer $ cycleEnum quality
         | key == Key2 -> do -- Material quality 
          quality <- rendererGetMaterialQuality renderer 
          rendererSetMaterialQuality renderer $ cycleEnum quality
         | key == Key3 -> -- Specular lighting
          rendererSetSpecularLighting renderer =<< fmap not (rendererGetSpecularLighting renderer)
         | key == Key4 -> -- Shadow rendering
          rendererSetDrawShadows renderer =<< fmap not (rendererGetDrawShadows renderer)
         | key == Key5 -> do -- Shadow map resolution
          shadowMapSize <- (*2) <$> rendererGetShadowMapSize renderer 
          rendererSetShadowMapSize renderer $ if shadowMapSize > 2048 then 512 else shadowMapSize
         | key == Key6 -> do -- Shadow depth and filtering quality
          quality <- rendererGetShadowQuality renderer
          rendererSetShadowQuality renderer $ cycleEnum quality
         | key == Key7 -> do -- Occlusion culling 
          occlusion <- (0 <) <$> rendererGetMaxOccluderTriangles renderer 
          rendererSetMaxOccluderTriangles renderer $ if not occlusion then 5000 else 0 
         | key == Key8 -> -- Instancing
          rendererSetDynamicInstancing renderer =<< rendererGetDynamicInstancing renderer
         | key == Key9 -> do -- Take screenshot
          cntx <- getContext app 
          withObject cntx $ \screenshot -> do 
            (graphics :: Ptr Graphics) <- fromJust <$> getSubsystem app
            graphicsTakeScreenShot graphics screenshot
            -- Here we save in the Data folder with data and time appended
            progDir <- getProgramDir fs 
            timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
            imageSavePNG screenshot $ progDir ++ "Data/Screenshot_" ++ timestamp ++ ".png"
         | otherwise -> return ()
    | otherwise -> return ()

cycleEnum :: (Eq a, Enum a, Bounded a) => a -> a 
cycleEnum a 
  | a == maxBound = minBound
  | otherwise = succ a

handleSceneUpdate :: Sample -> EventSceneUpdate -> IO ()
handleSceneUpdate s _ = do 
  let cameraNode = s ^. sampleCameraNode
      app = s ^. sampleApplication
  touchEnabled <- readIORef (s ^. sampleTouchEnabled)

  -- Move the camera by touch, if the camera node is initialized by descendant sample class
  when (touchEnabled && not (isNull cameraNode)) $ do 
    (input :: Ptr Input) <- fromJust <$> getSubsystem app 
    ni <- inputGetNumTouches input 
    forM_ [0 .. ni] $ \i -> do 
      state <- fromJust <$> inputGetTouch input i 
      unless (isNothing $ state ^. touchedElement) $ do -- touch on empty space
        if state^.touchDelta.x /= 0 || state^.touchDelta.y /= 0 then do 
          mcam <- nodeGetComponent cameraNode
          Monad.void $ whenJust mcam $ \(camera :: Ptr Camera) -> do 
            (graphics :: Ptr Graphics) <- fromJust <$> getSubsystem app 

            fov <- cameraGetFov camera 
            height <- fromIntegral <$> graphicsGetHeight graphics

            modifyIORef (s ^. sampleYaw) $ (+ ( touchSensitivity * fov / height * (fromIntegral $ state^.touchDelta.x)) )
            modifyIORef (s ^. samplePitch) $ (+ ( touchSensitivity * fov / height * (fromIntegral $ state^.touchDelta.y)) )
        
            -- Construct new orientation for the camera scene node from yaw and pitch; roll is fixed to zero
            yaw <- readIORef $ s^.sampleYaw
            pitch <- readIORef $ s^.samplePitch
            nodeSetRotation cameraNode $ quaternionFromEuler pitch yaw 0
        else do -- Move the cursor to the touch position
          (ui :: Ptr UI) <- fromJust <$> getSubsystem app
          cursor <- uiCursor ui 
          whenM (uiElementIsVisible cursor) $ uiElementSetPosition cursor $ state^.touchPosition
  
handleTouchBegin :: Sample -> EventTouchBegin -> IO ()
handleTouchBegin s _ = flip evalStateT s $ do 
  -- On some platforms like Windows the presence of touch input can only be detected dynamically
  initTouchInput
  app <- use sampleApplication
  unsubscribeFromEvent app (Proxy :: Proxy EventTouchBegin)