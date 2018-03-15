module Options where

import Control.Monad.Trans.Maybe ( MaybeT (..) )
import Control.Monad.IO.Class ( liftIO
                              , MonadIO (..)
                              )
import Data.Map.Strict ( (!?) )
import Options.Applicative ( ReadM (..)
                           , str
                           )

import qualified Brightness


brightnessDeviceReader :: ReadM (MaybeT IO Brightness.BrightnessDevice)
brightnessDeviceReader = do
    device <- str
    pure . MaybeT $ do
        devices <- liftIO Brightness.getAllDevices
        sequence $ Brightness.readDevice <$> devices !? device 
