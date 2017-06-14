module Settings where
import Data.AppSettings
       (Setting(..), DefaultConfig, setting, getDefaultConfig)

defaultUser :: Setting Int
defaultUser = Setting "defaultUser" 35682014

oAuthToken :: Setting String
oAuthToken = Setting "oAuthSecret" ""

oAuthSecret :: Setting String
oAuthSecret = Setting "oAuthToken" ""

setApiKey :: Setting String
setApiKey = Setting "apiKey" ""

setApiSecret :: Setting String
setApiSecret = Setting "apiSecret" ""

defaultConfig :: DefaultConfig
defaultConfig = getDefaultConfig $ do
    setting defaultUser