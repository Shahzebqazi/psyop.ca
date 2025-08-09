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

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as LBS

-- Define our API routes
type API = Get '[HTML] Html
      :<|> "about" :> Get '[HTML] Html
      :<|> "contact" :> Get '[HTML] Html
      :<|> "links" :> Get '[HTML] Html
      :<|> "admin" :> Get '[HTML] Html
      :<|> "static" :> Raw

-- HTML content type for Servant
data HTML

instance Accept HTML where
    contentType _ = "text/html"

instance MimeRender HTML Html where
    mimeRender _ = renderHtml

-- API implementation
server :: Server API
server = homePage
    :<|> aboutPage
    :<|> contactPage
    :<|> linksPage
    :<|> adminPage
    :<|> serveDirectoryFileServer "static"

-- Page handlers
homePage :: Handler Html
homePage = return $ pageTemplateHome "PSYOP - Home" $ do
    H.div ! A.class_ "hero-homepage" $ do
        -- Logo Section
        H.div ! A.class_ "logo-section" $ do
            H.img ! A.src "/static/psyop-logo.png" 
                  ! A.alt "PSYOP Logo"
                  ! A.class_ "main-logo"
        
        -- Navigation Menu
        H.nav ! A.class_ "hero-nav" $ do
            H.ul ! A.class_ "hero-nav-links" $ do
                H.li $ H.a ! A.href "/" $ "HOME"
                H.li $ H.a ! A.href "#" $ "LIVE"
                H.li $ H.a ! A.href "/about" $ "ABOUT"
                H.li $ H.a ! A.href "#" $ "MUSIC"
                H.li $ H.a ! A.href "/contact" $ "SOCIALS"
        
        -- Main Content Section
        H.div ! A.class_ "main-content-section" $ do
            H.div ! A.class_ "album-announcement" $ do
                H.h1 ! A.class_ "new-album-text" $ "NEW TRACK"
                H.h2 ! A.class_ "album-title-main" $ "MOONLIGHT PARADOX"
                H.a ! A.href "https://distrokid.com/hyperfollow/psyop21/moonlight-paradox" 
                    ! A.target "_blank"
                    ! A.rel "noopener noreferrer"
                    ! A.class_ "listen-button" $ "LISTEN NOW"



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
            H.h2 "Music Platforms"
            H.ul $ do
                H.li $ H.a ! A.href "#" $ "Spotify"
                H.li $ H.a ! A.href "#" $ "Bandcamp"
                H.li $ H.a ! A.href "#" $ "SoundCloud"
        H.div ! A.class_ "link-category" $ do
            H.h2 "Social Media"
            H.ul $ do
                H.li $ H.a ! A.href "#" $ "Instagram"
                H.li $ H.a ! A.href "#" $ "Twitter"
                H.li $ H.a ! A.href "#" $ "Facebook"



adminPage :: Handler Html
adminPage = return $ pageTemplate "PSYOP - Admin" $ do
    H.h1 "Admin Panel"
    H.p "Admin functionality coming soon..."

-- Homepage template (no header/footer)
pageTemplateHome :: String -> Html -> Html
pageTemplateHome title content = docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.title (toHtml title)
        H.link ! A.rel "stylesheet" ! A.href "/static/style.css"
        H.link ! A.rel "preconnect" ! A.href "https://fonts.googleapis.com"
        H.link ! A.rel "preconnect" ! A.href "https://fonts.gstatic.com"
        H.link ! A.rel "stylesheet" ! A.href "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&family=Rajdhani:wght@300;400;500;600;700&display=swap"
    H.body ! A.class_ "homepage-body" $ content

-- Common page template
pageTemplate :: String -> Html -> Html
pageTemplate title content = docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.title (toHtml title)
        H.link ! A.rel "stylesheet" ! A.href "/static/style.css"
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
        
        H.main ! A.class_ "main-content" $ content
        
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
startApp = run 8080 app