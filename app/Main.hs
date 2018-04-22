module Main where

import Data.Map.Strict
    ( (!) )
import Options.Applicative
    ( execParser )

import Brightness
    ( listDevices
    , displayBrightness
    , setBrightnessPercentage
    , getAllDevices
    , readDevice
    , addBrightnessValue
    )
import Options
    ( Options (..)
    , options
    , BrightnessCommand (..)
    )

run :: Options -> IO ()
run options' 
    = case options' of
          ListDevices -> listDevices =<< getAllDevices
          _ -> runOtherCommand options'

runOtherCommand :: Options -> IO ()
runOtherCommand OtherCommand
    { device = deviceName
    , otherCommand = otherCommand'
    } = do
        devices <- getAllDevices
        device' <- readDevice $ devices ! deviceName
        case otherCommand' of
            DisplayBrightness -> displayBrightness device'
            ChangeBrightness n -> setBrightnessPercentage n device'
            AddBrightnessValue n -> addBrightnessValue n device'

main :: IO ()
main = do
    options' <- execParser options
    run options'
