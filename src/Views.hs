{-# LANGUAGE OverloadedStrings #-}

module Views
    ( renderAsciiWallpaper
    , renderFallbackBackground
    , renderHomePage
    ) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A



-- ============================================================================
-- PURE HTML PRESENTATION COMPONENTS
-- ============================================================================

-- Render the ASCII wallpaper as HTML (takes pre-generated content)
renderAsciiWallpaper :: String -> Html
renderAsciiWallpaper asciiText = 
    H.div ! A.class_ "ascii-wallpaper" $ do
        H.pre ! A.class_ "ascii-content" $ H.toHtml asciiText



-- Render fallback gradient background
renderFallbackBackground :: Html
renderFallbackBackground = 
    H.div ! A.class_ "fallback-background" $ 
        H.div ! A.class_ "gradient-overlay" $ ""



-- Main view function that renders the home page
renderHomePage :: Html
renderHomePage = do
    H.div ! A.class_ "home-container" $ do
        -- ASCII wallpaper background (will be injected from Models.hs)
        H.div ! A.class_ "ascii-wallpaper-container" $ ""
        -- Band description card
        H.h1 ! A.class_ "black-title" $ do
            H.div ! A.class_ "bio-line bio-line-1" $ do
                H.em "Psyop"
                " (ˈsaɪ.ɑp) is a 4 piece Metal Machine from Toronto, Canada."
            H.div ! A.class_ "bio-line bio-line-2" $ "Rooted in nu-metal foundations with addictive hooks and ruthless breakdowns,"
            H.div ! A.class_ "bio-line bio-line-3" $ "With just the right progressive riffs and AAA Engineering."
            H.div ! A.class_ "bio-line bio-line-5" $ "Psyop is James, Max, Miles and Willy."
            H.div ! A.class_ "bio-line bio-line-6" $ do
                H.em "Psyop"
                " is short for:"
            H.div ! A.class_ "bio-line bio-line-7" $ do
                H.span ! A.class_ "psyop-definition" ! A.id "psyop-definition" $ "Psychological Operation - Military operation designed to influence emotions, attitudes, and behavior of target audiences to support national objectives."
            H.div ! A.class_ "bio-line bio-line-8" $ ""

        -- Music title
        H.h1 ! A.class_ "music-title" $ "Music"
        -- Content area with animations
        H.div ! A.class_ "content-area coming-soon-card" $ do
            H.div ! A.class_ "album-artwork" $ do
                H.img ! A.src "/assets/graphics/white/promo_1.jpg" 
                      ! A.alt "Great in Vain - Album Artwork"
                      ! A.class_ "album-cover"
            H.div ! A.class_ "album-title" $ do
                H.div ! A.class_ "title-line title-line-1" $ "psyop-02 August 2025"
                H.div ! A.class_ "title-line title-line-2" $ do
                    H.span ! A.class_ "title-text" $ H.u "Great in Vain"
                H.div ! A.class_ "title-line title-line-3" $ "~ A World Full of Pain ~"
            H.div ! A.class_ "coming-soon-overlay" $ "Coming Soon"
        
        -- Second Moonlight Paradox card (copy)
        H.div ! A.class_ "content-area" $ do
            H.div ! A.class_ "album-artwork" $ do
                H.a ! A.href "https://distrokid.com/hyperfollow/psyop-moonlight-paradox" 
                    ! A.target "_blank" 
                    ! A.rel "noopener noreferrer" $ do
                    H.img ! A.src "/assets/album-covers/single_spotify_soundcloud_bandcamp.jpg" 
                          ! A.alt "Moonlight Paradox - Album Artwork"
                          ! A.class_ "album-cover"
            H.div ! A.class_ "album-title" $ do
                H.div ! A.class_ "title-line title-line-1" $ "psyop-01 April 2025"
                H.div ! A.class_ "title-line title-line-2" $ do
                    H.a ! A.href "https://distrokid.com/hyperfollow/psyop-moonlight-paradox" 
                        ! A.target "_blank" 
                        ! A.rel "noopener noreferrer" 
                        ! A.class_ "title-link" $ "Moonlight Paradox"
                H.div ! A.class_ "title-line title-line-3" $ "~ Fall Back Down To Earth ~"



-- ============================================================================
-- EXPORT LIST - PURE HTML PRESENTATION FUNCTIONS ONLY
-- ============================================================================
