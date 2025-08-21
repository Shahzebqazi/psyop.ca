{-# LANGUAGE OverloadedStrings #-}

module Components.Footer
    ( Footer(..)
    , renderFooter
    , mkFooter
    , updateFooter
    ) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Text (Text)

-- Footer component state
data Footer = Footer
    { copyrightText :: String
    , showSocialLinks :: Bool
    , currentYear :: Int
    , socialLinks :: [SocialLink]
    } deriving (Show, Eq)

-- Social media link data
data SocialLink = SocialLink
    { platform :: Text
    , url :: Text
    , iconClass :: Text
    , displayName :: Text
    } deriving (Show, Eq)

-- Create a new footer with default social links
mkFooter :: Footer
mkFooter = Footer 
    { copyrightText = "© 2023-2025 Psyop. All rights reserved."
    , showSocialLinks = True
    , currentYear = 2025
    , socialLinks = defaultSocialLinks
    }

-- Default social media links
defaultSocialLinks :: [SocialLink]
defaultSocialLinks = 
    [ SocialLink "spotify" "https://open.spotify.com/artist/psyop" "icon-spotify" "Spotify"
    , SocialLink "soundcloud" "https://soundcloud.com/psyop" "icon-soundcloud" "SoundCloud"
    , SocialLink "bandcamp" "https://psyop.bandcamp.com" "icon-bandcamp" "Bandcamp"
    , SocialLink "instagram" "https://instagram.com/psyop" "icon-instagram" "Instagram"
    , SocialLink "youtube" "https://youtube.com/@psyop" "icon-youtube" "YouTube"
    , SocialLink "tiktok" "https://tiktok.com/@psyop" "icon-tiktok" "TikTok"
    ]

-- Render the footer component
renderFooter :: Footer -> Html
renderFooter footerComponent = 
    H.footer ! A.class_ "main-footer" $ do
        H.div ! A.class_ "footer-content" $ do
            renderFooterMain footerComponent
            renderFooterBottom footerComponent

-- Render main footer content
renderFooterMain :: Footer -> Html
renderFooterMain footer = 
    H.div ! A.class_ "footer-main" $ do
        renderFooterBrand

-- Render footer brand section
renderFooterBrand :: Html
renderFooterBrand = 
    H.div ! A.class_ "footer-brand" $ do
        H.p ! A.class_ "footer-description" $ "Psychological operations designed to influence perception through sonic manipulation and strategic deployment of electronic warfare tactics."

-- Render footer links section
renderFooterLinks :: Footer -> Html
renderFooterLinks _ = 
    H.div ! A.class_ "footer-links" $ do
        H.h4 "Quick Links"
        H.ul $ do
            H.li $ H.a ! A.href "/" $ "Home"
            H.li $ H.a ! A.href "/music" $ "Music"
            H.li $ H.a ! A.href "/shows" $ "Shows"
            H.li $ H.a ! A.href "/contact" $ "Contact"

-- Render footer social links
renderFooterSocial :: Footer -> Html
renderFooterSocial footer = 
    H.div ! A.class_ "footer-social" $ do
        H.h4 "Follow PSYOP"
        H.div ! A.class_ "social-links" $ do
            mapM_ renderSocialLink (socialLinks footer)

-- Render individual social link
renderSocialLink :: SocialLink -> Html
renderSocialLink social = 
    H.a ! A.href (toValue $ url social)
       ! A.class_ (toValue $ "social-link " <> iconClass social)
       ! A.target "_blank"
       ! A.rel "noopener noreferrer"
       ! A.title (toValue $ displayName social) $ 
        H.i ! A.class_ (toValue $ "icon " <> iconClass social) $ ""

-- Render footer bottom section
renderFooterBottom :: Footer -> Html
renderFooterBottom footer = 
    H.div ! A.class_ "footer-bottom" $ do
        H.div ! A.class_ "footer-copyright" $ do
            "© 2023-2025 "
            H.em "Psyop"
            ". All our rights have been violated."
        H.div ! A.class_ "footer-legal" $ do
            H.a ! A.href "/privacy" $ "Privacy Policy"

-- Update footer state
updateFooter :: Footer -> Footer -> Footer
updateFooter oldFooter newFooter = newFooter
