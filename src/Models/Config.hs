module Models.Config
    ( Config(..)
    , defaultConfig
    , getServerPort
    , getAssetPath
    , getPublicPath
    ) where

import System.Environment (lookupEnv)

-- Configuration data structure
data Config = Config
    { serverPort :: Int
    , assetPath :: FilePath
    , publicPath :: FilePath
    , devMode :: Bool
    } deriving (Show, Eq)

-- Default configuration
defaultConfig :: Config
defaultConfig = Config
    { serverPort = 8080
    , assetPath = "assets"
    , publicPath = "public"
    , devMode = True
    }

-- Configuration getters
getServerPort :: Config -> Int
getServerPort = serverPort

getAssetPath :: Config -> FilePath
getAssetPath = assetPath

getPublicPath :: Config -> FilePath
getPublicPath = publicPath

-- Environment-based configuration (commented out until needed)
-- getConfig :: IO Config
-- getConfig = do
--     port <- maybe 8080 read <$> lookupEnv "PSYOP_PORT"
--     devMode <- maybe True read <$> lookupEnv "PSYOP_DEV_MODE"
--     return defaultConfig
--         { serverPort = port
--         , devMode = devMode
--         }
