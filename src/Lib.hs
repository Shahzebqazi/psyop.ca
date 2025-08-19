{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html (preEscapedToHtml)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy

-- Simple configuration
data Config = Config
    { serverPort :: Int
    } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config { serverPort = 8080 }

-- Define API routes
type API = Get '[HTML] Html
      :<|> "home" :> Get '[HTML] Html
      :<|> "music" :> Get '[HTML] Html
      :<|> "links" :> Get '[HTML] Html
      :<|> "shows" :> Get '[HTML] Html
      :<|> "about" :> Get '[HTML] Html
      :<|> "contact" :> Get '[HTML] Html
      :<|> "health" :> Get '[HTML] Html
      :<|> "css" :> Raw
      :<|> "assets" :> Raw
      :<|> Raw

-- HTML content type for Servant
data HTML

instance Accept HTML where
    contentType _ = "text/html"

instance MimeRender HTML Html where
    mimeRender _ = renderHtml

-- API implementation
server :: Server API
server = homePage
    :<|> homePage
    :<|> musicPage
    :<|> linksPage
    :<|> showsPage
    :<|> aboutPage
    :<|> contactPage
    :<|> healthPage
    :<|> serveDirectoryFileServer "public/css"
    :<|> serveDirectoryFileServer "assets"
    :<|> serveDirectoryFileServer "public"

-- Page handlers
homePage :: Handler Html
homePage = liftIO $ readFile "public/index.html" >>= return . preEscapedToHtml

musicPage :: Handler Html
musicPage = liftIO $ readFile "public/index.html" >>= return . preEscapedToHtml

showsPage :: Handler Html
showsPage = return $ pageTemplate "PSYOP - Upcoming Shows" $ do
    H.div ! A.class_ "content-section" $ do
        H.h1 "Upcoming Shows"
        H.p "Check back soon for upcoming live performances and events."
        H.div ! A.class_ "shows-placeholder" $ do
            H.p "No upcoming shows scheduled at this time."
            H.p "Follow us on social media for updates!"

aboutPage :: Handler Html
aboutPage = return $ pageTemplate "PSYOP - About" $ do
    H.div ! A.class_ "content-section" $ do
        H.h1 "About PSYOP"
        H.p "PSYOP is an experimental electronic music project that explores the intersection of technology, consciousness, and sound. Our music delves into cyberpunk aesthetics and futuristic soundscapes."
        H.p "Born from the digital underground, PSYOP creates immersive audio experiences that challenge conventional boundaries and transport listeners to alternate realities."
        H.p "Each track is carefully crafted to evoke emotions and memories that exist in the liminal space between human and machine consciousness."

contactPage :: Handler Html
contactPage = return $ pageTemplate "PSYOP - Contact" $ do
    H.h1 "Get In Touch"
    H.div ! A.class_ "contact-info" $ do
        H.p $ do
            "Email: "
            H.a ! A.href "mailto:admin@psyop.ca" $ "admin@psyop.ca"
    H.div ! A.class_ "social-links" $ do
        H.a ! A.href "#" ! A.class_ "social-link" $ "Instagram"
        H.a ! A.href "#" ! A.class_ "social-link" $ "Twitter"
        H.a ! A.href "#" ! A.class_ "social-link" $ "Facebook"

linksPage :: Handler Html
linksPage = return $ pageTemplate "PSYOP - Links" $ do
    H.h1 "Links"
    H.div ! A.class_ "links-grid" $ do
        H.div ! A.class_ "link-category" $ do
            H.h2 "Contact"
            H.div ! A.class_ "contact-links" $ do
                H.div ! A.class_ "contact-item" $ do
                    H.img ! A.src "/assets/icons/contact/email.svg" ! A.alt "Email" ! A.class_ "contact-icon"
                    H.a ! A.href "mailto:info@psyop.ca" $ "info@psyop.ca"
                H.div ! A.class_ "contact-item" $ do
                    H.img ! A.src "/assets/icons/contact/email.svg" ! A.alt "Email" ! A.class_ "contact-icon"
                    H.a ! A.href "mailto:booking@psyop.ca" $ "booking@psyop.ca"
        
        H.div ! A.class_ "link-category" $ do
            H.h2 "Music Platforms"
            H.div ! A.class_ "platform-links" $ do
                H.div ! A.class_ "platform-item" $ do
                    H.img ! A.src "/assets/icons/streaming/spotify.svg" ! A.alt "Spotify" ! A.class_ "platform-icon"
                    H.a ! A.href "https://open.spotify.com/artist/psyop" ! A.target "_blank" ! A.rel "noopener noreferrer" $ "Spotify"
                H.div ! A.class_ "platform-item" $ do
                    H.img ! A.src "/assets/icons/streaming/bandcamp.svg" ! A.alt "Bandcamp" ! A.class_ "platform-icon"
                    H.a ! A.href "https://psyop.bandcamp.com" ! A.target "_blank" ! A.rel "noopener noreferrer" $ "Bandcamp"
                H.div ! A.class_ "platform-item" $ do
                    H.img ! A.src "/assets/icons/streaming/soundcloud.svg" ! A.alt "SoundCloud" ! A.class_ "platform-icon"
                    H.a ! A.href "https://soundcloud.com/psyop" ! A.target "_blank" ! A.rel "noopener noreferrer" $ "SoundCloud"
                H.div ! A.class_ "platform-item" $ do
                    H.img ! A.src "/assets/icons/streaming/apple_music.svg" ! A.alt "Apple Music" ! A.class_ "platform-icon"
                    H.a ! A.href "https://music.apple.com/artist/psyop" ! A.target "_blank" ! A.rel "noopener noreferrer" $ "Apple Music"
        
        H.div ! A.class_ "link-category" $ do
            H.h2 "Social Media"
            H.div ! A.class_ "social-links" $ do
                H.div ! A.class_ "social-item" $ do
                    H.img ! A.src "/assets/icons/social/instagram.svg" ! A.alt "Instagram" ! A.class_ "social-icon"
                    H.a ! A.href "https://instagram.com/psyopband" ! A.target "_blank" ! A.rel "noopener noreferrer" $ "Instagram"
                H.div ! A.class_ "social-item" $ do
                    H.img ! A.src "/assets/icons/social/twitter.svg" ! A.alt "Twitter" ! A.class_ "social-icon"
                    H.a ! A.href "https://twitter.com/psyopband" ! A.target "_blank" ! A.rel "noopener noreferrer" $ "Twitter"
                H.div ! A.class_ "social-item" $ do
                    H.img ! A.src "/assets/icons/social/facebook.svg" ! A.alt "Facebook" ! A.class_ "social-icon"
                    H.a ! A.href "https://facebook.com/psyopband" ! A.target "_blank" ! A.rel "noopener noreferrer" $ "Facebook"

healthPage :: Handler Html
healthPage = return $ pageTemplate "PSYOP - Health Check" $ do
    H.div ! A.class_ "content-section" $ do
        H.h1 "System Health"
        H.div ! A.class_ "health-status" $ do
            H.h2 "All Systems Operational"
            H.p $ do
                "Server running on port " 
                H.span ! A.class_ "port-number" $ toHtml (show $ serverPort defaultConfig)

-- Common page template
pageTemplate :: String -> Html -> Html
pageTemplate pageTitle pageContent = docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.title (toHtml pageTitle)
        H.link ! A.rel "stylesheet" ! A.href "/css/style.css"
        H.link ! A.rel "preconnect" ! A.href "https://fonts.googleapis.com"
        H.link ! A.rel "preconnect" ! A.href "https://fonts.gstatic.com"
        H.link ! A.rel "stylesheet" ! A.href "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&family=Rajdhani:wght@300;400;500;600;700&display=swap"
    H.body $ do
        H.header ! A.class_ "main-header" $ do
            H.nav ! A.class_ "main-nav" $ do
                H.div ! A.class_ "nav-container" $ do
                    H.div ! A.class_ "nav-brand" $ do
                        H.a ! A.href "/" ! A.class_ "brand-logo" $ "PSYOP"
                    H.ul ! A.class_ "nav-links" $ do
                        H.li $ H.a ! A.href "/" $ "Home"
                        H.li $ H.a ! A.href "/about" $ "About"
                        H.li $ H.a ! A.href "/contact" $ "Contact"
                        H.li $ H.a ! A.href "/links" $ "Links"
        
        H.main ! A.class_ "main-content" $ pageContent
        
        H.footer ! A.class_ "main-footer" $ do
            H.div ! A.class_ "footer-content" $ do
                H.div ! A.class_ "footer-section" $ do
                    H.h3 "PSYOP"
                    H.p "Pushing the boundaries of electronic music"
                H.div ! A.class_ "footer-section" $ do
                    H.h4 "Connect"
                    H.p $ do
                        H.a ! A.href "mailto:admin@psyop.ca" $ "admin@psyop.ca"
                H.div ! A.class_ "footer-section" $ do
                    H.p ! A.class_ "footer-copyright" $ "© 2025 PSYOP. All rights reserved."

-- WAI Application
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- Start the server
startApp :: IO ()
startApp = run (serverPort defaultConfig) app