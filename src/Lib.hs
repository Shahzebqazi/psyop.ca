{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Controllers.PageController
import Models.Config (defaultConfig, getServerPort)
import Models.ImageSequence (testNoRepetition)

-- HTML content type for Servant
data HTML

instance Accept HTML where
    contentType _ = "text/html"

instance MimeRender HTML Html where
    mimeRender _ = renderHtml

-- Define API routes - clean and focused
type API = Get '[HTML] Html
      :<|> "home" :> Get '[HTML] Html
      :<|> "music" :> Get '[HTML] Html
      :<|> "links" :> Get '[HTML] Html
      :<|> "shows" :> Get '[HTML] Html
      :<|> "about" :> Get '[HTML] Html
      :<|> "contact" :> Get '[HTML] Html
      :<|> "test" :> Get '[HTML] Html
      :<|> "health" :> Get '[HTML] Html
      :<|> "css" :> Raw
      :<|> "assets" :> Raw
      :<|> Raw

-- API implementation using clean controllers
server :: Server API
server = handleHomePage
    :<|> handleHomePage
    :<|> handleMusicPage
    :<|> handleLinksPage
    :<|> handleShowsPage
    :<|> handleAboutPage
    :<|> handleContactPage
    :<|> handleTestPage
    :<|> handleHealthPage
    :<|> serveDirectoryFileServer "public/css"
    :<|> serveDirectoryFileServer "assets"
    :<|> serveDirectoryFileServer "public"

-- WAI Application
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- Start the server with configuration
startApp :: IO ()
startApp = do
    putStrLn $ "Starting PSYOP website on http://localhost:" ++ show (getServerPort defaultConfig)
    run (getServerPort defaultConfig) app