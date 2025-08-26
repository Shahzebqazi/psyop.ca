{-# LANGUAGE OverloadedStrings #-}

module Models
    ( defaultConfig
    , getServerPort
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.IO (readFile)
import qualified Prelude as P
-- import Text.Blaze.Html.Renderer.String (renderHtml)
-- import qualified Components.WebGLBackground as WebGL

-- ============================================================================
-- SERVER CONFIGURATION
-- ============================================================================

-- Default server configuration
data ServerConfig = ServerConfig
    { serverPort :: P.Int
    , serverHost :: Text
    , environment :: Text
    } deriving (P.Show, P.Eq)

-- Default configuration
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
    { serverPort = 8080
    , serverHost = "localhost"
    , environment = "development"
    }

-- Get server port from config
getServerPort :: ServerConfig -> P.Int
getServerPort = serverPort

-- ============================================================================
