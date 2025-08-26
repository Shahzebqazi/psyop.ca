{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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
        H.script ! A.src "/js/webgl-background.js" ! A.defer ""
        H.script ! A.src "/js/menu.js" ! A.defer ""

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

-- | Home page view with bio
homeView :: Html
homeView = H.div ! A.class_ "page home-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.div ! A.class_ "hero-section" $ do
            H.h1 ! A.class_ "hero-title" $ "PSYOP"
            H.p ! A.class_ "hero-subtitle" $ "ˈsaɪ.ɑp"
            H.p ! A.class_ "hero-description" $ "4‑piece Metal Machine from Toronto, Canada"
        
        H.div ! A.class_ "bio-section" $ do
            H.h2 $ "About"
            H.div ! A.class_ "bio-content" $ do
                H.p $ "PSYOP is a dynamic metal band pushing the boundaries of heavy music. With influences ranging from classic metal to modern progressive sounds, we create an intense and immersive musical experience."
                H.p $ "Our sound combines crushing riffs, technical precision, and atmospheric elements to deliver a unique take on contemporary metal."
                H.p $ "Based in Toronto, we're actively performing and recording new material. Stay tuned for upcoming releases and tour dates."
        
        H.div ! A.class_ "cta-section" $ do
            H.h3 $ "Listen Now"
            H.div ! A.class_ "cta-buttons" $ do
                H.a ! A.href "/music" ! A.class_ "cta-button primary" $ "Stream Music"
                H.a ! A.href "/tour" ! A.class_ "cta-button secondary" $ "See Tour Dates"

-- | Music page view with streaming links
musicView :: Html
musicView = H.div ! A.class_ "page music-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.h1 $ "Music"
        H.p ! A.class_ "page-description" $ "Stream PSYOP on your favorite platforms"
        
        H.div ! A.class_ "streaming-platforms" $ do
            H.h2 $ "Streaming Platforms"
            H.div ! A.class_ "platform-grid" $ do
                H.div ! A.class_ "platform-card" $ do
                    H.h3 $ "Spotify"
                    H.p $ "Listen to our latest tracks and albums"
                    H.a ! A.href "https://open.spotify.com/artist/psyop" ! A.target "_blank" ! A.class_ "platform-link" $ "Listen on Spotify"
                
                H.div ! A.class_ "platform-card" $ do
                    H.h3 $ "Apple Music"
                    H.p $ "Stream our music on Apple Music"
                    H.a ! A.href "https://music.apple.com/artist/psyop" ! A.target "_blank" ! A.class_ "platform-link" $ "Listen on Apple Music"
                
                H.div ! A.class_ "platform-card" $ do
                    H.h3 $ "Bandcamp"
                    H.p $ "Support us directly on Bandcamp"
                    H.a ! A.href "https://psyop.bandcamp.com" ! A.target "_blank" ! A.class_ "platform-link" $ "Visit Bandcamp"
                
                H.div ! A.class_ "platform-card" $ do
                    H.h3 $ "YouTube"
                    H.p $ "Watch our music videos and live performances"
                    H.a ! A.href "https://youtube.com/@psyopband" ! A.target "_blank" ! A.class_ "platform-link" $ "Watch on YouTube"
        
        H.div ! A.class_ "latest-release" $ do
            H.h2 $ "Latest Release"
            H.div ! A.class_ "release-info" $ do
                H.h3 $ "Album Title"
                H.p $ "Release Date: Coming Soon"
                H.p $ "Track listing and details will be available here"

-- | Tour page view with dates
tourView :: Html
tourView = H.div ! A.class_ "page tour-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.h1 $ "Tour"
        H.p ! A.class_ "page-description" $ "See PSYOP live"
        
        H.div ! A.class_ "tour-dates" $ do
            H.h2 $ "Upcoming Shows"
            H.div ! A.class_ "tour-list" $ do
                H.div ! A.class_ "tour-item" $ do
                    H.div ! A.class_ "tour-date" $ "Coming Soon"
                    H.div ! A.class_ "tour-location" $ "More dates to be announced"
                    H.div ! A.class_ "tour-venue" $ "Stay tuned for updates"
            
            H.div ! A.class_ "tour-cta" $ do
                H.p $ "Want us to play your venue? Get in touch!"
                H.a ! A.href "/contact" ! A.class_ "cta-button" $ "Contact Us"

-- | Contact page view
contactView :: Html
contactView = H.div ! A.class_ "page contact-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.h1 $ "Contact"
        H.p ! A.class_ "page-description" $ "Get in touch with PSYOP"
        
        H.div ! A.class_ "contact-info" $ do
            H.div ! A.class_ "contact-section" $ do
                H.h2 $ "General Inquiries"
                H.p $ H.a ! A.href "mailto:info@psyop.ca" $ "info@psyop.ca"
                H.p $ "For all general questions and information"
            
            H.div ! A.class_ "contact-section" $ do
                H.h2 $ "Booking"
                H.p $ H.a ! A.href "mailto:booking@psyop.ca" $ "booking@psyop.ca"
                H.p $ "For show bookings and event inquiries"
            
            H.div ! A.class_ "contact-section" $ do
                H.h2 $ "Press & Media"
                H.p $ H.a ! A.href "mailto:press@psyop.ca" $ "press@psyop.ca"
                H.p $ "For press inquiries and media requests"
        
        H.div ! A.class_ "social-links-contact" $ do
            H.h2 $ "Follow Us"
            H.div ! A.class_ "social-grid" $ do
                H.a ! A.href "https://instagram.com/psyopband" ! A.target "_blank" ! A.class_ "social-card instagram" $ do
                    H.h3 $ "Instagram"
                    H.p $ "Latest photos and updates"
                
                H.a ! A.href "https://twitter.com/psyopband" ! A.target "_blank" ! A.class_ "social-card twitter" $ do
                    H.h3 $ "Twitter"
                    H.p $ "News and announcements"
                
                H.a ! A.href "https://facebook.com/psyopband" ! A.target "_blank" ! A.class_ "social-card facebook" $ do
                    H.h3 $ "Facebook"
                    H.p $ "Connect with our community"

-- | Privacy Policy page
privacyView :: Html
privacyView = H.div ! A.class_ "page privacy-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.h1 $ "Privacy Policy"
        H.div ! A.class_ "policy-content" $ do
            H.p $ "Last updated: 2025"
            H.h2 $ "Information We Collect"
            H.p $ "We collect information you provide directly to us, such as when you contact us or sign up for our newsletter."
            H.h2 $ "How We Use Your Information"
            H.p $ "We use the information we collect to provide, maintain, and improve our services."
            H.h2 $ "Contact Us"
            H.p $ "If you have questions about this Privacy Policy, please contact us at info@psyop.ca"

-- | Terms of Service page
termsView :: Html
termsView = H.div ! A.class_ "page terms-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.h1 $ "Terms of Service"
        H.div ! A.class_ "terms-content" $ do
            H.p $ "Last updated: 2025"
            H.h2 $ "Acceptance of Terms"
            H.p $ "By accessing and using this website, you accept and agree to be bound by the terms and provision of this agreement."
            H.h2 $ "Use License"
            H.p $ "Permission is granted to temporarily download one copy of the materials on PSYOP's website for personal, non-commercial transitory viewing only."
            H.h2 $ "Contact Us"
            H.p $ "If you have questions about these Terms of Service, please contact us at info@psyop.ca"

-- | Error page for 404s
errorView :: Html
errorView = H.div ! A.class_ "page error-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.h1 $ "404 - Page Not Found"
        H.p $ "The page you're looking for doesn't exist."
        H.a ! A.href "/" ! A.class_ "cta-button" $ "Go Home"

-- | Helper function to render HTML to String
renderView :: Html -> String
renderView = TL.unpack . renderHtml
