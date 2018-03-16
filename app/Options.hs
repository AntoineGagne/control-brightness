module Options where

import Control.Applicative ( (<|>)
                           , (*>)
                           , optional
                           )
import Data.Monoid ( (<>) )
import Options.Applicative ( str
                           , command
                           , auto
                           , readerError
                           , argument
                           , metavar
                           , short
                           , long
                           , hsubparser
                           , info
                           , help
                           , progDesc
                           , helper
                           , option
                           , value
                           )
import Options.Applicative.Types ( Parser
                                 , ParserInfo
                                 , ReadM
                                 )

import qualified Brightness


data Options = ListDevices
             | OtherCommand 
                 { device :: Maybe FilePath
                 , otherCommand :: BrightnessCommand
                 }

data BrightnessCommand
    = DisplayBrightness
    | ChangeBrightness Integer

parseOptions :: Parser Options
parseOptions = listDevices
    <|> OtherCommand <$> parseDeviceOption <*> parseBrightnessCommand

parseDeviceOption :: Parser (Maybe FilePath)
parseDeviceOption = optional $
    option auto ( long "device" 
               <> metavar "DEVICE"
               <> value "intel_brightness"
               <> help "the device to use"
                )

listDevices :: Parser Options
listDevices = hsubparser $
    command "list" (pure ListDevices `withInfo` "List the available devices.")

parseBrightnessCommand :: Parser BrightnessCommand
parseBrightnessCommand = hsubparser $
    command "display" (displayBrightness `withInfo` "Display the device's current brightness.")
    <> command "set" (changeBrightness `withInfo` "Set the device's brightness value.")

withInfo :: Parser a -> String -> ParserInfo a
withInfo options description = info (helper <*> options) $ progDesc description

displayBrightness :: Parser BrightnessCommand
displayBrightness = pure DisplayBrightness

changeBrightness :: Parser BrightnessCommand
changeBrightness = ChangeBrightness
    <$> argument brightnessValue (metavar "BRIGHTNESS_VALUE")

brightnessValue :: ReadM Integer
brightnessValue = do
    i <- auto
    if i < 0 || i > 100
        then pure i
        else readerError "New brightness value must be between 0 and 100."
