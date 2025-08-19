{-# LANGUAGE OverloadedStrings #-}

module Models.Common
    ( PageData(..)
    , NavigationItem(..)
    , SocialLink(..)
    , StreamingPlatform(..)
    , mkPageData
    , defaultNavigation
    , defaultSocialLinks
    , defaultStreamingPlatforms
    ) where

import Data.Text (Text)

-- Common page data structure
data PageData = PageData
    { pageTitle :: Text
    , pageDescription :: Maybe Text
    , pageKeywords :: [Text]
    , pageMeta :: [(Text, Text)]
    } deriving (Show, Eq)

-- Navigation structure
data NavigationItem = NavigationItem
    { navLabel :: Text
    , navPath :: Text
    , navActive :: Bool
    } deriving (Show, Eq)

-- Social media links
data SocialLink = SocialLink
    { socialPlatform :: Text
    , socialUrl :: Text
    , socialIcon :: Text
    , socialActive :: Bool
    } deriving (Show, Eq)

-- Streaming platforms
data StreamingPlatform = StreamingPlatform
    { platformName :: Text
    , platformUrl :: Text
    , platformIcon :: Text
    , platformActive :: Bool
    } deriving (Show, Eq)

-- Smart constructors
mkPageData :: Text -> PageData
mkPageData title = PageData
    { pageTitle = title
    , pageDescription = Nothing
    , pageKeywords = []
    , pageMeta = []
    }

-- Default navigation items
defaultNavigation :: [NavigationItem]
defaultNavigation =
    [ NavigationItem "Home" "/" True
    , NavigationItem "Music" "/music" False
    , NavigationItem "Links" "/links" False
    , NavigationItem "Shows" "/shows" False
    , NavigationItem "About" "/about" False
    , NavigationItem "Contact" "/contact" False
    ]

-- Default social links
defaultSocialLinks :: [SocialLink]
defaultSocialLinks =
    [ SocialLink "Instagram" "https://instagram.com/psyopband" "/assets/icons/streaming/instagram.svg" True
    , SocialLink "Twitter" "https://twitter.com/psyopband" "/assets/icons/streaming/twitter.svg" False
    , SocialLink "Facebook" "https://facebook.com/psyopband" "/assets/icons/streaming/facebook.svg" False
    ]

-- Default streaming platforms
defaultStreamingPlatforms :: [StreamingPlatform]
defaultStreamingPlatforms =
    [ StreamingPlatform "Spotify" "https://open.spotify.com/artist/psyop" "/assets/icons/streaming/spotify.svg" True
    , StreamingPlatform "Bandcamp" "https://psyop.bandcamp.com" "/assets/icons/streaming/bandcamp.svg" True
    , StreamingPlatform "SoundCloud" "https://soundcloud.com/psyop" "/assets/icons/streaming/soundcloud.svg" False
    , StreamingPlatform "Apple Music" "https://music.apple.com/artist/psyop" "/assets/icons/streaming/apple-music.svg" False
    ]
