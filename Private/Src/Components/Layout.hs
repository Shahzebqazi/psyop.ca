{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Components.Layout where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T

-- | Main layout template with WebGL background
mainLayout :: T.Text -> Html -> Html
mainLayout title content = html $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.title $ toHtml title
        H.meta ! A.name "description" ! A.content "PSYOP (ˈsaɪ.ɑp) is a 4‑piece Metal Machine from Toronto, Canada."
        H.meta ! A.property "og:title" ! A.content "PSYOP"
        H.meta ! A.property "og:description" ! A.content "PSYOP (ˈsaɪ.ɑp) is a 4‑piece Metal Machine from Toronto, Canada."
        H.meta ! A.property "og:type" ! A.content "website"
        H.meta ! A.property "og:url" ! A.content "https://psyop.ca"
        H.link ! A.rel "stylesheet" ! A.href "/css/main.css"
        H.link ! A.rel "stylesheet" ! A.href "/css/responsive.css"
        H.link ! A.rel "stylesheet" ! A.href "/css/components.css"
        H.script ! A.src "/js/webgl-background.js" ! A.defer "" $ ""
        H.script ! A.src "/js/menu.js" ! A.defer "" $ ""

    H.body $ do
        -- WebGL Background Canvas
        H.canvas ! A.id "webgl-background" ! A.class_ "webgl-canvas" $ ""
        
        -- Header with Navigation
        headerView
        
        -- Main Content
        H.main ! A.class_ "main-content" $ content
        
        -- Footer
        footerView
        
        -- Mobile Menu Overlay
        mobileMenuView

-- | Header with responsive navigation
headerView :: Html
headerView = H.header ! A.class_ "header" $ do
    H.div ! A.class_ "header-container" $ do
        -- Logo/Brand
        H.div ! A.class_ "brand" $ do
            H.a ! A.href "/" ! A.class_ "brand-link" $ do
                H.span ! A.class_ "brand-text" $ "PSYOP"
                H.span ! A.class_ "brand-subtitle" $ "Metal Machine"
        
        -- Desktop Navigation
        H.nav ! A.class_ "nav-desktop" $ do
            H.ul ! A.class_ "nav-list" $ do
                H.li $ H.a ! A.href "/" ! A.class_ "nav-link" $ "Home"
                H.li $ H.a ! A.href "/music" ! A.class_ "nav-link" $ "Music"
                H.li $ H.a ! A.href "/tour" ! A.class_ "nav-link" $ "Tour"
                H.li $ H.a ! A.href "/contact" ! A.class_ "nav-link" $ "Contact"
        
        -- Mobile Menu Button
        H.button ! A.class_ "mobile-menu-btn" ! A.id "mobile-menu-toggle" $ do
            H.span ! A.class_ "hamburger-line" $ ""
            H.span ! A.class_ "hamburger-line" $ ""
            H.span ! A.class_ "hamburger-line" $ ""

-- | Mobile menu overlay
mobileMenuView :: Html
mobileMenuView = H.div ! A.class_ "mobile-menu" ! A.id "mobile-menu" $ do
    H.nav ! A.class_ "mobile-nav" $ do
        H.ul ! A.class_ "mobile-nav-list" $ do
            H.li $ H.a ! A.href "/" ! A.class_ "mobile-nav-link" $ "Home"
            H.li $ H.a ! A.href "/music" ! A.class_ "mobile-nav-link" $ "Music"
            H.li $ H.a ! A.href "/tour" ! A.class_ "mobile-nav-link" $ "Tour"
            H.li $ H.a ! A.href "/contact" ! A.class_ "mobile-nav-link" $ "Contact"

-- | Footer with links and info
footerView :: Html
footerView = H.footer ! A.class_ "footer" $ do
    H.div ! A.class_ "footer-container" $ do
        H.div ! A.class_ "footer-content" $ do
            -- Contact Info
            H.div ! A.class_ "footer-section" $ do
                H.h3 $ "Contact"
                H.p $ H.a ! A.href "mailto:info@psyop.ca" $ "info@psyop.ca"
            
            -- Social Links
            H.div ! A.class_ "footer-section" $ do
                H.h3 $ "Follow"
                H.div ! A.class_ "social-links" $ do
                    H.a ! A.href "https://instagram.com/psyopband" ! A.target "_blank" ! A.class_ "social-link" $ "Instagram"
                    H.a ! A.href "https://twitter.com/psyopband" ! A.target "_blank" ! A.class_ "social-link" $ "Twitter"
                    H.a ! A.href "https://facebook.com/psyopband" ! A.target "_blank" ! A.class_ "social-link" $ "Facebook"
            
            -- Legal Links
            H.div ! A.class_ "footer-section" $ do
                H.h3 $ "Legal"
                H.div ! A.class_ "legal-links" $ do
                    H.a ! A.href "/privacy" ! A.class_ "legal-link" $ "Privacy Policy"
                    H.a ! A.href "/terms" ! A.class_ "legal-link" $ "Terms of Service"
        
        H.div ! A.class_ "footer-bottom" $ do
            H.p $ "© 2025 PSYOP. All rights reserved."
