{-# LANGUAGE QuasiQuotes #-}
module Sample(
    Sample
  , SampleRef
  , newSample
  , runSample
  , setLogoVisible
  , initMouseMode
  -- | Lenses
  , sampleApplication
  , sampleName
  , sampleYaw
  , samplePitch
  , sampleTouchEnabled
  , sampleScreenJoystickIndex
  , sampleScreenSettingsIndex
  , samplePaused
  , sampleLogo
  , sampleScene
  , sampleCameraNode
  , sampleJoystickPatch
  -- | Helpers
  , joysticPatch
  , fromJustTrace
  , whenNothing
  , clamp
  , vec3
  ) where

import Control.Lens hiding (Context)
import Control.Monad as Monad
import Data.IORef
import Data.Maybe
import Data.Proxy
import Data.StateVar
import Data.Thyme
import Foreign
import Graphics.Urho3D
import System.Locale
import Text.RawString.QQ

import Internal.Sample

type SampleRef = IORef Sample

touchSensitivity :: Float
touchSensitivity = 2.0

runSample :: SampleRef -> IO ()
runSample sr = do
  s <- readIORef sr
  applicationRun $ s ^. sampleApplication

newSample :: Ptr Context
  -> String -- ^ Sample name
  -> String -- ^ Joystick patch string
  -> (SampleRef -> IO ()) -- ^ Custom start function
  -> IO SampleRef
newSample context sname joystickPatch customStart = do
  sampleRef <- newIORef undefined
  app <- newSharedObject ApplicationCreate {
      appCreateContext = context
    , appCreateSetup = const $ sampleSetup sampleRef
    , appCreateStart = const $ sampleStart sampleRef >> customStart sampleRef
    , appCreateStop = const $ sampleStop sampleRef
    }
  let s = Sample {
    _sampleApplication = app
  , _sampleName = sname
  , _sampleYaw = 0
  , _samplePitch = 0
  , _sampleTouchEnabled = False
  , _sampleScreenSettingsIndex = maxBound
  , _sampleScreenJoystickIndex = maxBound
  , _samplePaused = False
  , _sampleLogo = makePointer nullPtr
  , _sampleScene = makePointer nullPtr
  , _sampleCameraNode = makePointer nullPtr
  , _sampleJoystickPatch = joystickPatch
  , _sampleMouseMode = MM'Absolute
  }
  writeIORef sampleRef s
  return sampleRef

sampleSetup :: SampleRef -> IO ()
sampleSetup sr = do
  s <- readIORef sr
  let app = s^.sampleApplication

  startupParameter app "WindowTitle" $= s^.sampleName

  fs :: Ptr FileSystem <- fromJustTrace "sampleSetup:FileSystem" <$> getSubsystem app
  prefDir <- getAppPreferencesDir fs "urho3d" "logs"
  startupParameter app "LogName" $= prefDir ++ s^.sampleName ++ ".log"

  startupParameter app "FullScreen" $= False
  startupParameter app "Headless" $= False
  startupParameter app "ForceGL2" $= True
  startupParameter app "Sound" $= False

sampleStart :: SampleRef -> IO ()
sampleStart sr = do
  s <- readIORef sr
  let app = s^.sampleApplication

  if platform == "Android" || platform == "iOS"
  then initTouchInput sr
  else do
    is :: Ptr Input <- fromJustTrace "sampleStart:InputSystem" <$> getSubsystem app
    jcount <- inputGetNumJoysticks is
    when (jcount == 0) $ subscribeToEvent app $ handleTouchBegin sr

  createLogo sr
  setWindowTitleAndIcon sr
  createConsoleAndDebugHud sr

  subscribeToEvent app $ handleKeyDown sr
  subscribeToEvent app $ handleKeyUp sr
  subscribeToEvent app $ handleSceneUpdate sr

initTouchInput :: SampleRef -> IO ()
initTouchInput sr = do
  s <- readIORef sr
  let app = s^.sampleApplication

  liftIO $ modifyIORef' sr (set sampleTouchEnabled True)

  resCache :: Ptr ResourceCache <- fromJustTrace "initTouchInput:ResourceCache" <$> getSubsystem app
  input :: Ptr Input <- fromJustTrace "initTouchInput:InputSystem" <$> getSubsystem app
  layoutM <- cacheGetResource resCache "UI/ScreenJoystick_Samples.xml" True
  case layoutM of
    Nothing -> error "Cannot open UI/ScreenJoystick_Samples.xml"
    Just layout -> do
      let joystickPatch = s ^. sampleJoystickPatch
      unless (null joystickPatch) $ do
        cntx <- getContext app
        withObject cntx $ \patchFile ->
          whenM (xmlFileFromString patchFile joystickPatch) $ xmlFilePatch layout patchFile

      defStyleM <- cacheGetResource resCache "UI/DefaultStyle.xml" True
      _ <- whenJust defStyleM $ \defStyle -> do
        j <- inputAddScreenJoystick input layout defStyle
        inputSetScreenJoystickVisible input j True
      return ()

initMouseMode :: SampleRef -> MouseMode -> IO ()
initMouseMode sr mode = do
  modifyIORef' sr (set sampleMouseMode mode)
  s <- readIORef sr
  let app = s ^. sampleApplication

  input :: Ptr Input <- fromJustTrace "handleKeyDown:InputSystem" <$> getSubsystem app
  when (mode == MM'Free) $ inputSetMouseVisible input True False

  console :: Ptr Console <- fromJustTrace "handleKeyUp:Console" <$> getSubsystem app
  when (mode /= MM'Absolute) $ do
    inputSetMouseMode input mode False
    whenM (consoleIsVisible console) $ inputSetMouseMode input MM'Absolute True


setLogoVisible :: SampleRef -> Bool -> IO ()
setLogoVisible sr flag = do
  s <- readIORef sr
  let ptr = s ^. sampleLogo
  unless (isNull ptr) $ uiElementSetVisible ptr flag

sampleStop :: SampleRef -> IO ()
sampleStop sr = do
  s <- readIORef sr
  eng <- applicationEngine $ s ^. sampleApplication
  engineDumpResources eng True

createLogo :: SampleRef -> IO ()
createLogo sr = do
  s <- readIORef sr
  let app = s ^. sampleApplication
  cache :: Ptr ResourceCache <- fromJustTrace "createLogo:ResourceCache" <$> getSubsystem app

  logoTextureM <- cacheGetResource cache "Textures/LogoLarge.png" True
  case logoTextureM of
    Nothing -> return ()
    Just (logoTexture :: Ptr Texture2D) -> do
      ui :: Ptr UI <- fromJustTrace "createLogo:UI" <$> getSubsystem app
      sprite <- createChildSimple =<< uiRoot ui
      liftIO $ modifyIORef' sr $ set sampleLogo sprite

      -- Set logo sprite texture
      spriteSetTexture sprite logoTexture
      twidth <- textureWidth logoTexture
      theight <- textureHeight logoTexture
      -- Set logo sprite scale
      spriteSetScale sprite $ 256 / fromIntegral twidth
      -- Set logo sprite size
      uiElementSetSize sprite $ IntVector2 twidth theight
      -- Set logo sprite hot spot
      spriteSetHotSpot' sprite 0 theight
      -- Set logo sprite alignment
      uiElementSetAlignment sprite AlignmentLeft AlignmentBottom
      -- Make logo not fully opaque to show the scene underneath
      uiElementSetOpacity sprite 0.75
      -- Set a low priority for the logo so that other UI elements can be drawn on top
      uiElementSetPriority sprite (-100)

setWindowTitleAndIcon :: SampleRef -> IO ()
setWindowTitleAndIcon sr = do
  s <- readIORef sr
  let app = s ^. sampleApplication
  cache :: Ptr ResourceCache <- fromJustTrace "setWindowTitleAndIcon:ResourceCache" <$> getSubsystem app
  graphics :: Ptr Graphics <- fromJustTrace "setWindowTitleAndIcon:Graphics" <$> getSubsystem app
  iconM <- cacheGetResource cache "Textures/UrhoIcon.png" True
  _ <- whenJust iconM $ graphicsSetWindowIcon graphics
  graphicsSetWindowTitle graphics "Urho3D Sample"

createConsoleAndDebugHud :: SampleRef -> IO ()
createConsoleAndDebugHud sr = do
  s <- readIORef sr
  let app = s ^. sampleApplication
  engine <- applicationEngine app

  cache :: Ptr ResourceCache <- fromJustTrace "createConsoleAndDebugHud:ResourceCache" <$> getSubsystem app
  xmlFileM <- cacheGetResource cache "UI/DefaultStyle.xml" True
  _ <- whenJust xmlFileM $ \xmlFile -> do
    consoleM <- engineCreateConsole engine
    _ <- whenJust consoleM $ \console -> do
      consoleSetDefaultStyle console xmlFile
      bg <- consoleGetBackground console
      uiElementSetOpacity bg 0.8

    debugHud <- engineCreateDebugHud engine
    debugHudSetDefaultStyle debugHud xmlFile

  return ()

handleKeyUp :: SampleRef -> EventKeyUp -> IO ()
handleKeyUp sr EventKeyUp{..}
  | upKey == KeyEsc = do -- Close console (if open) or exit when ESC is pressed
    s <- readIORef sr
    let app = s ^. sampleApplication
    console :: Ptr Console <- fromJustTrace "handleKeyUp:Console" <$> getSubsystem app
    isVisible <- consoleIsVisible console
    if isVisible then consoleSetVisible console False
      else do
        engine <- applicationEngine app
        engineExit engine
  | otherwise = pure ()

handleKeyDown :: SampleRef -> EventKeyDown -> IO ()
handleKeyDown sr EventKeyDown{..} = do
  s <- readIORef sr
  let app = s ^. sampleApplication
      k = pressKey
  engine <- applicationEngine app
  console :: Ptr Console <- fromJustTrace "handleKeyDown:Console" <$> getSubsystem app
  debugHud :: Ptr DebugHud <- fromJustTrace "handleKeyDown:DebugHud" <$> getSubsystem app
  ui :: Ptr UI <- fromJustTrace "handleKeyDown:UI" <$> getSubsystem app
  focusElem <- uiFocusElement ui
  input :: Ptr Input <- fromJustTrace "handleKeyDown:InputSystem" <$> getSubsystem app
  cache :: Ptr ResourceCache <- fromJustTrace "handleKeyDown:ResourceCache" <$> getSubsystem app
  fs :: Ptr FileSystem <- fromJustTrace "handleKeyDown:FileSystem" <$> getSubsystem app

  if| k == KeyEsc -> do
      -- Close console (if open) or exit when ESC is pressed
      vis <- consoleIsVisible console
      if vis
        then consoleSetVisible console False
        else engineExit engine
    | k == KeyF1 -> consoleToggle console  -- Toggle console with F1
    | k == KeyF2 -> debugHudToggle debugHud -- Toggle debug HUD with F2
    | isNothing focusElem -> do -- Common rendering quality controls, only when UI has no focused element
      renderer :: Ptr Renderer <- fromJustTrace "handleKeyDown:Renderer" <$> getSubsystem app

      -- Preferences / Pause
      let touchEnabled = s ^. sampleTouchEnabled
      if | k == KeySelect && touchEnabled -> do
          modifyIORef' sr $ over samplePaused not

          let settIndex = s ^. sampleScreenSettingsIndex
          if | settIndex == maxBound -> do
                -- Lazy initialization
                layoutM <- cacheGetResource cache "UI/ScreenJoystickSettings_Samples.xml" True
                styleM <- cacheGetResource cache "UI/DefaultStyle.xml" True
                _ <- whenJust (liftM2 (,) layoutM styleM) $ \(layout, style) -> do
                  ji <- inputAddScreenJoystick input layout style
                  modifyIORef' sr $ set sampleScreenSettingsIndex ji
                return ()
             | otherwise -> inputSetScreenJoystickVisible input settIndex $ s ^. samplePaused
         | k == Key1 -> do -- Texture quality
          quality <- rendererGetTextureQuality renderer
          rendererSetTextureQuality renderer $ cycleEnum quality
         | k == Key2 -> do -- Material quality
          quality <- rendererGetMaterialQuality renderer
          rendererSetMaterialQuality renderer $ cycleEnum quality
         | k == Key3 -> -- Specular lighting
          rendererSetSpecularLighting renderer =<< fmap not (rendererGetSpecularLighting renderer)
         | k == Key4 -> -- Shadow rendering
          rendererSetDrawShadows renderer =<< fmap not (rendererGetDrawShadows renderer)
         | k == Key5 -> do -- Shadow map resolution
          shadowMapSize <- (*2) <$> rendererGetShadowMapSize renderer
          rendererSetShadowMapSize renderer $ if shadowMapSize > 2048 then 512 else shadowMapSize
         | k == Key6 -> do -- Shadow depth and filtering quality
          quality <- rendererGetShadowQuality renderer
          rendererSetShadowQuality renderer $ cycleEnum quality
         | k == Key7 -> do -- Occlusion culling
          occlusion <- (0 <) <$> rendererGetMaxOccluderTriangles renderer
          rendererSetMaxOccluderTriangles renderer $ if not occlusion then 5000 else 0
         | k == Key8 -> -- Instancing
          rendererSetDynamicInstancing renderer =<< rendererGetDynamicInstancing renderer
         | k == Key9 -> do -- Take screenshot
          cntx <- getContext app
          withObject cntx $ \screenshot -> do
            graphics :: Ptr Graphics <- fromJustTrace "handleKeyDown:Graphics" <$> getSubsystem app
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

handleSceneUpdate :: SampleRef -> EventSceneUpdate -> IO ()
handleSceneUpdate sr _ = do
  s <- readIORef sr
  let cameraNode = s ^. sampleCameraNode
      app = s ^. sampleApplication
  let touchEnabled = s ^. sampleTouchEnabled

  -- Move the camera by touch, if the camera node is initialized by descendant sample class
  when (touchEnabled && not (isNull cameraNode)) $ do
    input :: Ptr Input <- fromJustTrace "handleSceneUpdate:InputSystem" <$> getSubsystem app
    ni <- inputGetNumTouches input
    forM_ [0 .. ni] $ \i -> do
      tstate <- fromJustTrace "handleSceneUpdate:TouchState" <$> inputGetTouch input i
      unless (isNull $ tstate ^. touchedElement) $  -- touch on empty space
        if tstate^.touchDelta.x /= 0 || tstate^.touchDelta.y /= 0 then do
          mcam <- nodeGetComponent cameraNode False
          Monad.void $ whenJust mcam $ \(cam :: Ptr Camera) -> do
            graphics :: Ptr Graphics <- fromJustTrace "handleSceneUpdate:Graphics" <$> getSubsystem app

            fov <- cameraGetFov cam
            h <- fromIntegral <$> graphicsGetHeight graphics

            modifyIORef' sr $ over sampleYaw (+ ( touchSensitivity * fov / h * fromIntegral (tstate^.touchDelta.x)) )
            modifyIORef' sr $ over samplePitch (+ ( touchSensitivity * fov / h * fromIntegral (tstate^.touchDelta.y)) )

            -- Construct new orientation for the camera scene node from yaw and pitch; roll is fixed to zero
            let yaw = s^.sampleYaw
            let pitch = s^.samplePitch
            nodeSetRotation cameraNode $ quaternionFromEuler pitch yaw 0
        else do -- Move the cursor to the touch position
          ui :: Ptr UI <- fromJustTrace "handleSceneUpdate:UI" <$> getSubsystem app
          cursor <- uiCursor ui
          whenM (uiElementIsVisible cursor) $ uiElementSetPosition cursor $ tstate^.touchPosition

handleTouchBegin :: SampleRef -> EventTouchBegin -> IO ()
handleTouchBegin sr _ = do
  -- On some platforms like Windows the presence of touch input can only be detected dynamically
  s <- readIORef sr
  initTouchInput sr
  let app = s ^. sampleApplication
  unsubscribeFromEvent app (Proxy :: Proxy EventTouchBegin)

-- | Helper function that prints profided message when get Nothing
fromJustTrace :: String -> Maybe a -> a
fromJustTrace msg Nothing = error $ "fromJust: " ++ msg
fromJustTrace _ (Just a) = a

-- | Helper to run code when value is nothing
whenNothing :: Monad m => Maybe a -> b -> m b -> m b
whenNothing Nothing _ f = f
whenNothing (Just _) a _ = return a

vec3 :: Float -> Vector3
vec3 a = Vector3 a a a

-- | Return XML patch instructions for screen joystick layout for a specific sample app, if any.
joysticPatch :: String
joysticPatch = [r|
<patch>
    <remove sel="/element/element[./attribute[@name='Name' and @value='Button1']]/attribute[@name='Is Visible']" />
    <replace sel="/element/element[./attribute[@name='Name' and @value='Button1']]/element[./attribute[@name='Name' and @value='Label']]/attribute[@name='Text']/@value">Debug</replace>
    <add sel="/element/element[./attribute[@name='Name' and @value='Button1']]">
        <element type="Text">
            <attribute name="Name" value="KeyBinding" />
            <attribute name="Text" value="SPACE" />
        </element>
    </add>
</patch>
|]
