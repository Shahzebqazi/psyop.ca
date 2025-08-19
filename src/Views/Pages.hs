{-# LANGUAGE OverloadedStrings #-}

module Views.Pages
    ( renderHomePage
    , renderMusicPage
    , renderLinksPage
    , renderShowsPage
    , renderAboutPage
    , renderContactPage
    ) where

import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Models.Music (getAvailableTracks, MusicTrack(..), TrackStatus(..))

-- Home page with polaroid gallery
renderHomePage :: Html
renderHomePage = do
    -- Home page content removed

-- Music page with track listings (NO POLAROID)
renderMusicPage :: Html
renderMusicPage = do
    H.h1 "Music"
    H.div ! A.class_ "music-tracks" $ do
        mapM_ renderTrack (getAvailableTracks)

renderTrack :: MusicTrack -> Html
renderTrack track = H.div ! A.class_ "track-item" $ do
    H.div ! A.class_ "track-artwork" $ do
        H.img ! A.src (toValue $ trackArtwork track) ! A.alt (toValue $ trackTitle track)
    H.div ! A.class_ "track-info" $ do
        H.h3 (toHtml $ trackTitle track)
        H.p ! A.class_ "track-status" $ 
            case trackStatus track of
                ComingSoon -> "Coming Soon"
                Available -> "Available Now"
                Released -> "Released"
        case trackStatus track of
            Available -> H.a ! A.href "#" ! A.class_ "track-link" $ "Listen Now"
            _ -> H.span ! A.class_ "track-link disabled" $ "Coming Soon"

-- Links page with social media and streaming links (NO POLAROID)
renderLinksPage :: Html
renderLinksPage = do
    H.h1 "Connect with PSYOP"
    H.div ! A.class_ "links-grid" $ do
        H.div ! A.class_ "link-category" $ do
            H.h2 "Streaming Platforms"
            H.div ! A.class_ "platform-links" $ do
                H.div ! A.class_ "platform-item" $ do
                    H.img ! A.src "/assets/icons/streaming/spotify.svg" ! A.alt "Spotify"
                    H.a ! A.href "https://open.spotify.com/artist/psyop" ! A.target "_blank" $ "Spotify"
                H.div ! A.class_ "platform-item" $ do
                    H.img ! A.src "/assets/icons/streaming/bandcamp.svg" ! A.alt "Bandcamp"
                    H.a ! A.href "https://psyop.bandcamp.com" ! A.target "_blank" $ "Bandcamp"
        
        H.div ! A.class_ "link-category" $ do
            H.h2 "Social Media"
            H.div ! A.class_ "social-links" $ do
                H.div ! A.class_ "social-item" $ do
                    H.img ! A.src "/assets/icons/streaming/instagram.svg" ! A.alt "Instagram"
                    H.a ! A.href "https://instagram.com/psyopband" ! A.target "_blank" $ "Instagram"

-- Shows page (NO POLAROID)
renderShowsPage :: Html
renderShowsPage = do
    H.h1 "Upcoming Shows"
    H.div ! A.class_ "shows-content" $ do
        H.p "Check back soon for upcoming live performances and events."
        H.div ! A.class_ "shows-placeholder" $ do
            H.p "No upcoming shows scheduled at this time."
            H.p "Follow us on social media for updates!"

-- About page (NO POLAROID)
renderAboutPage :: Html
renderAboutPage = do
    H.h1 "About PSYOP"
    H.div ! A.class_ "about-content" $ do
        H.p "PSYOP is an experimental electronic music project that explores the intersection of technology, consciousness, and sound."
        H.p "Our music delves into cyberpunk aesthetics and futuristic soundscapes."
        H.p "Born from the digital underground, PSYOP creates immersive audio experiences that challenge conventional boundaries."

-- Contact page (NO POLAROID)
renderContactPage :: Html
renderContactPage = do
    H.h1 "Get In Touch"
    H.div ! A.class_ "contact-content" $ do
        H.div ! A.class_ "contact-info" $ do
            H.p $ do
                "Email: "
                H.a ! A.href "mailto:admin@psyop.ca" $ "admin@psyop.ca"
        H.div ! A.class_ "contact-form" $ do
            H.form ! A.action "/contact" ! A.method "POST" $ do
                H.input ! A.type_ "text" ! A.name "name" ! A.placeholder "Your Name" ! A.required ""
                H.input ! A.type_ "email" ! A.name "email" ! A.placeholder "Your Email" ! A.required ""
                H.textarea ! A.name "message" ! A.placeholder "Your Message" ! A.required ""
                H.button ! A.type_ "submit" $ "Send Message"
