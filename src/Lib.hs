{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( startApp
    , app
    , ImageSequence(..)
    , createImageSequence
    , nextImage
    , getCurrentImage
    , getSequenceStats
    , testNoRepetition
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.List (nub)
import Control.Monad.IO.Class (liftIO)

-- Image Sequence Model
data ImageSequence = ImageSequence
    { totalImages :: Int
    , currentIndex :: Int
    , usedIndices :: [Int]
    , sequenceOrder :: [Int]
    } deriving (Show, Eq)

-- Create a new image sequence with n unique items
createImageSequence :: Int -> ImageSequence
createImageSequence n = ImageSequence
    { totalImages = n
    , currentIndex = 0
    , usedIndices = []
    , sequenceOrder = [0..n-1]
    }

-- Get the next image index without repetition
nextImage :: ImageSequence -> ImageSequence
nextImage imgSeq@(ImageSequence n current used order)
    | length used >= n = imgSeq { usedIndices = [], currentIndex = 0 }  -- Reset when all used
    | otherwise = 
        let nextIdx = order !! current
            newUsed = nextIdx : used
            newCurrent = (current + 1) `mod` n
        in imgSeq 
            { currentIndex = newCurrent
            , usedIndices = newUsed
            }

-- Get the current image index
getCurrentImage :: ImageSequence -> Int
getCurrentImage = currentIndex

-- Get statistics about the sequence
getSequenceStats :: ImageSequence -> (Int, Int, [Int])
getSequenceStats (ImageSequence n current used _) = (n, current, used)

-- Test function to verify no repetition within n items
testNoRepetition :: Int -> Bool
testNoRepetition n = 
    let imgSeq = createImageSequence n
        -- Check that the first n images are all different
        firstNImages = take n $ iterate nextImage imgSeq
        firstNIndices = Prelude.map getCurrentImage firstNImages
        hasRepetition = length firstNIndices /= length (nub firstNIndices)
    in not hasRepetition

-- Define our API routes
type API = Get '[HTML] Html
      :<|> "home" :> Get '[HTML] Html
      :<|> "music" :> Get '[HTML] Html
      :<|> "links" :> Get '[HTML] Html
      :<|> "shows" :> Get '[HTML] Html
      :<|> "about" :> Get '[HTML] Html
      :<|> "contact" :> Get '[HTML] Html
      :<|> "admin" :> Get '[HTML] Html
      :<|> "test" :> Get '[HTML] Html
      :<|> "css" :> Raw
      :<|> "public" :> Raw

-- HTML content type for Servant
data HTML

instance Accept HTML where
    contentType _ = "text/html"

instance MimeRender HTML Html where
    mimeRender _ = renderHtml

-- API implementation
server :: Server API
server = homePage
    :<|> homePage      -- /home
    :<|> musicPage     -- /music  
    :<|> linksPage     -- /links
    :<|> showsPage     -- /shows
    :<|> aboutPage
    :<|> contactPage
    :<|> adminPage
    :<|> testPage
    :<|> serveDirectoryFileServer "css"
    :<|> serveDirectoryFileServer "public"

-- Page handlers
homePage :: Handler Html
homePage = do
    -- Serve the actual HTML content from public/index.html
    liftIO $ readFile "public/index.html" >>= return . toHtml

musicPage :: Handler Html
musicPage = return $ pageTemplate "PSYOP - Music" $ do
    H.div ! A.class_ "content-section" $ do
        H.h1 "Music"
        H.p "Discover PSYOP's latest tracks and releases."
        H.div ! A.class_ "music-grid" $ do
            H.div ! A.class_ "track-item" $ do
                H.h3 "MOONLIGHT PARADOX"
                H.p "Latest release - Available now on all platforms"
                H.a ! A.href "https://distrokid.com/hyperfollow/psyop21/moonlight-paradox" 
                    ! A.target "_blank"
                    ! A.rel "noopener noreferrer"
                    ! A.class_ "listen-button" $ "LISTEN NOW"

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

testPage :: Handler Html
testPage = return $ pageTemplate "PSYOP - Image Sequence Tests" $ do
    H.h1 "Image Sequence Model Tests"
    H.h2 "Testing Non-Repetition for Different Sizes"
    
    H.div ! A.class_ "test-results" $ do
        H.h3 "Test Results:"
        H.ul $ do
            H.li $ do
                H.strong "10 items: "
                H.span ! A.class_ (if testNoRepetition 10 then "pass" else "fail") $ 
                    if testNoRepetition 10 then "PASS" else "FAIL"
            H.li $ do
                H.strong "50 items: "
                H.span ! A.class_ (if testNoRepetition 50 then "pass" else "fail") $ 
                    if testNoRepetition 50 then "PASS" else "FAIL"
            H.li $ do
                H.strong "100 items: "
                H.span ! A.class_ (if testNoRepetition 100 then "pass" else "fail") $ 
                    if testNoRepetition 100 then "PASS" else "FAIL"
    
    H.h2 "Sequence Examples:"
    H.div ! A.class_ "sequence-examples" $ do
        H.h3 "10-item sequence (first 20 steps):"
        H.pre $ toHtml $ show $ take 20 $ Prelude.map getCurrentImage $ iterate nextImage (createImageSequence 10)
        
        H.h3 "50-item sequence (first 20 steps):"
        H.pre $ toHtml $ show $ take 20 $ Prelude.map getCurrentImage $ iterate nextImage (createImageSequence 50)
        
        H.h3 "100-item sequence (first 20 steps):"
        H.pre $ toHtml $ show $ take 20 $ Prelude.map getCurrentImage $ iterate nextImage (createImageSequence 100)
    
    H.h2 "Lovecraftian Transition Test:"
    H.div ! A.class_ "lovecraftian-test" $ do
        H.p "Scroll on the main page to see Lovecraftian horror transitions between images!"
        H.p "The transitions feature:"
        H.ul $ do
            H.li "Floating red eyes (representing eldritch watchers)"
            H.li "Dark mouths/voids (representing cosmic horrors)"
            H.li "Writhing tentacles (representing otherworldly entities)"
            H.li "Ethereal whispers (subtle white orbs)"
        H.p "All elements are created using pure CSS/SVG - no external images required!"

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
startApp = run 8080 app