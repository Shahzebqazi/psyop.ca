{-# LANGUAGE OverloadedStrings #-}

module Views.Components
    ( renderNavigation
    , renderFooter
    , renderSocialLinks
    , renderStreamingPlatforms
    , renderButton
    , renderCard
    , renderIcon
    ) where

import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Data.Text (Text)
import Models.Common (NavigationItem(..), SocialLink(..), StreamingPlatform(..))

-- Reusable navigation component
renderNavigation :: [NavigationItem] -> Html
renderNavigation navItems = H.nav ! A.class_ "main-nav" $ do
    H.div ! A.class_ "nav-container" $ do
        H.div ! A.class_ "nav-brand" $ do
            H.a ! A.href "/" ! A.class_ "brand-logo" $ "PSYOP"
        H.ul ! A.class_ "nav-links" $ do
            mapM_ renderNavItem navItems

renderNavItem :: NavigationItem -> Html
renderNavItem item = H.li $ do
    let classes = if navActive item then "nav-link active" else "nav-link"
    H.a ! A.href (toValue $ navPath item) ! A.class_ classes $ toHtml (navLabel item)

-- Reusable footer component
renderFooter :: Html
renderFooter = H.footer ! A.class_ "main-footer" $ do
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

-- Reusable social links component
renderSocialLinks :: [SocialLink] -> Html
renderSocialLinks socialLinks = H.div ! A.class_ "social-links" $ do
    mapM_ renderSocialLink socialLinks

renderSocialLink :: SocialLink -> Html
renderSocialLink social = H.div ! A.class_ "social-item" $ do
    H.img ! A.src (toValue $ socialIcon social) ! A.alt (toValue $ socialPlatform social)
    H.a ! A.href (toValue $ socialUrl social) ! A.target "_blank" $ toHtml (socialPlatform social)

-- Reusable streaming platforms component
renderStreamingPlatforms :: [StreamingPlatform] -> Html
renderStreamingPlatforms platforms = H.div ! A.class_ "platform-links" $ do
    mapM_ renderStreamingPlatform platforms

renderStreamingPlatform :: StreamingPlatform -> Html
renderStreamingPlatform platform = H.div ! A.class_ "platform-item" $ do
    H.img ! A.src (toValue $ platformIcon platform) ! A.alt (toValue $ platformName platform)
    H.a ! A.href (toValue $ platformUrl platform) ! A.target "_blank" $ toHtml (platformName platform)

-- Reusable button component
renderButton :: Text -> Text -> Text -> Html
renderButton text url classes = H.a ! A.href (toValue url) ! A.class_ (toValue classes) $ toHtml text

-- Reusable card component
renderCard :: Text -> Html -> Html
renderCard title content = H.div ! A.class_ "card" $ do
    H.div ! A.class_ "card-header" $ H.h3 (toHtml title)
    H.div ! A.class_ "card-body" $ content

-- Reusable icon component
renderIcon :: Text -> Text -> Html
renderIcon iconPath altText = H.img ! A.src (toValue iconPath) ! A.alt (toValue altText) ! A.class_ "icon"
