{-# LANGUAGE TemplateHaskell #-}

module Brightness
    ( listDevices
    , displayBrightness
    , setBrightnessPercentage
    , readDevice
    , getAllDevices
    , BrightnessDevice
    , addBrightnessValue
    ) where

import Control.Concurrent.Async
    ( Concurrently (..) )
import Control.Lens
    ( makeLenses
    , (^.)
    , (.~)
    , (&)
    )
import Data.Map.Strict 
    ( Map
    , insert
    , empty
    , union
    , keys
    )
import System.EasyFile
    ( (</>)
    , takeBaseName
    )
import System.FilePath.Find
    ( fileType
    , find
    , depth
    , (==?)
    , (/=?)
    , (||?)
    , FileType
        ( Directory
        , SymbolicLink
        )
    )

data BrightnessDevice = BrightnessDevice
    { _actualBrightness :: Integer
    , _maximumBrightness :: Integer
    , _brightness :: Integer
    , _device :: FilePath
    } deriving (Show)
makeLenses ''BrightnessDevice

listDevices :: Map FilePath FilePath -> IO ()
listDevices devices = mapM_ putStrLn $ keys devices

displayBrightness :: BrightnessDevice -> IO ()
displayBrightness = print . brightnessPercentage

addBrightnessValue :: Integer -> BrightnessDevice -> IO ()
addBrightnessValue n device = setBrightness (normalizeValue (device^.brightness + n)) device

normalizeValue :: Integer -> Integer
normalizeValue = max 0 . min 100

setBrightnessPercentage :: Integer -> BrightnessDevice -> IO ()
setBrightnessPercentage n device = setBrightness newBrightness device
  where
    newBrightness = normalizeValue n'
    n' = floor $ fromInteger n / 100 * fromInteger (device^.maximumBrightness) / fromInteger (device^.brightness)

setBrightness :: Integer -> BrightnessDevice -> IO ()
setBrightness n device = writeDevice (device & brightness .~ n)

brightnessPercentage :: BrightnessDevice -> Integer
brightnessPercentage device = floor $
    fromInteger (device^.actualBrightness ) / fromInteger (device^.maximumBrightness) * 100

writeDevice :: BrightnessDevice -> IO ()
writeDevice device' =
    writeFile (device'^.device </> "brightness") (show (device'^.brightness))

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
getAllDevices =
    foldr1 union <$> mapM getControllerDevices controllers
  where
    controllers = ["/sys/class/backlight/", "/sys/class/leds/"]

getControllerDevices :: FilePath -> IO (Map FilePath FilePath)
getControllerDevices controllerPath =
    foldr f empty <$> find (depth ==? 0) (isDirectory ||? isSymbolicLink) controllerPath
  where
    f device m = 
        let name = takeBaseName device
        in if not . null $ name then insert name (controllerPath </> device) m else m
    isDirectory = fileType ==? Directory
    isSymbolicLink = fileType ==? SymbolicLink
