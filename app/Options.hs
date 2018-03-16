module Options
    ( options
    , Options (..)
    , BrightnessCommand (..)
    ) where

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
                           , (<**>)
                           , fullDesc
                           , header
                           , infoOption
                           )
import Options.Applicative.Types ( Parser
                                 , ParserInfo
                                 , ReadM
                                 )

import qualified Brightness


data Options = ListDevices
             | OtherCommand 
                 { device :: FilePath
                 , otherCommand :: BrightnessCommand
                 }
                 deriving (Show)

data BrightnessCommand
    = DisplayBrightness
    | ChangeBrightness Integer
    deriving (Show)

options :: ParserInfo Options
options = 
    info (parseOptions <**> helper <**> versionOptions)
         (fullDesc
        <> progDesc "Controls screen's brightness."
        <> header "control-brightness"
         )
  where
      versionOptions = 
          infoOption "control-brightness v0.0.0" (long "version" <> help "display the version")

parseOptions :: Parser Options
parseOptions = listDevices
    <|> OtherCommand <$> parseDeviceOption <*> parseBrightnessCommand

parseDeviceOption :: Parser FilePath
parseDeviceOption = option auto ( long "device"
                               <> metavar "DEVICE"
                               <> value "intel_backlight"
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
    if i >= 0 && i <= 100
        then pure i
        else readerError "New brightness value must be between 0 and 100."
