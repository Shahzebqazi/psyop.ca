{-# LANGUAGE OverloadedStrings #-}

module Components.Pages.Music where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Components.UI.Cards (platformCard)

-- | Music page view with streaming links
musicView :: Html
musicView = H.div ! A.class_ "page music-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.h1 $ "Music"
        H.p ! A.class_ "page-description" $ "Stream PSYOP on your favorite platforms"
        
        streamingPlatformsSection
        latestReleaseSection

-- | Streaming platforms section
streamingPlatformsSection :: Html
streamingPlatformsSection = H.div ! A.class_ "streaming-platforms" $ do
    H.h2 $ "Streaming Platforms"
    H.div ! A.class_ "platform-grid" $ do
        platformCard "https://open.spotify.com/artist/psyop" "Spotify" "Listen to our latest tracks and albums" "Listen on Spotify"
        platformCard "https://music.apple.com/artist/psyop" "Apple Music" "Stream our music on Apple Music" "Listen on Apple Music"
        platformCard "https://psyop.bandcamp.com" "Bandcamp" "Support us directly on Bandcamp" "Visit Bandcamp"
        platformCard "https://youtube.com/@psyopband" "YouTube" "Watch our music videos and live performances" "Watch on YouTube"

-- | Latest release section
latestReleaseSection :: Html
latestReleaseSection = H.div ! A.class_ "latest-release" $ do
    H.h2 $ "Latest Release"
    H.div ! A.class_ "release-info" $ do
        H.h3 $ "Album Title"
        H.p $ "Release Date: Coming Soon"
        H.p $ "Track listing and details will be available here"
