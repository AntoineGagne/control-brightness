module Brightness where

import Control.Concurrent.Async ( Concurrently (..) )
import Data.Map.Strict ( Map
                       , insert
                       , empty
                       , union
                       , keys
                       )
import System.EasyFile ( (</>)
                       , takeBaseName
                       )
import System.FilePath.Find ( fileType
                            , find
                            , depth
                            , (==?)
                            , (/=?)
                            , (||?)
                            , FileType ( Directory
                                       , SymbolicLink
                                       )
                            )

data BrightnessDevice = BrightnessDevice
    { actualBrightness :: Integer
    , maximumBrightness :: Integer
    , brightness :: Integer
    , device :: FilePath
    } deriving (Show)

listDevices :: Map FilePath FilePath -> IO ()
listDevices devices = mapM_ putStrLn $ keys devices

setBrightness :: Integer -> BrightnessDevice -> IO ()
setBrightness level device =
    let device' = device { brightness = level } in writeDevice device'

brightnessPercentage :: BrightnessDevice -> Double
brightnessPercentage device = 
    fromInteger (actualBrightness device) / fromInteger (maximumBrightness device) * 100

writeDevice :: BrightnessDevice -> IO ()
writeDevice BrightnessDevice
    { brightness = brightness'
    , device = path'
    } = writeFile (path' </> "brightness") (show brightness')

readDevice :: FilePath -> IO BrightnessDevice
readDevice devicePath = runConcurrently $
    BrightnessDevice
        <$> Concurrently (readIntegerFromFile "actual_brightness")
        <*> Concurrently (readIntegerFromFile "max_brightness")
        <*> Concurrently (readIntegerFromFile "brightness")
        <*> Concurrently (pure devicePath)
  where
      readIntegerFromFile path = read <$> readFile (devicePath </> path)

getAllDevices :: IO (Map FilePath FilePath)
getAllDevices = do
    devices <- mapM getControllerDevices controllers
    pure $ foldr1 union devices
  where
    controllers = ["/sys/class/backlight/", "/sys/class/leds/"]


getControllerDevices :: FilePath -> IO (Map FilePath FilePath)
getControllerDevices controllerPath = do
    devices <- find (depth ==? 0) (isDirectory ||? isSymbolicLink) controllerPath
    pure $ foldr f empty devices
  where
    f device m = 
        let name = takeBaseName device
        in if not . null $ name then insert name (controllerPath </> device) m else m
    isDirectory = fileType ==? Directory
    isSymbolicLink = fileType ==? SymbolicLink
