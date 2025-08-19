{-# LANGUAGE OverloadedStrings #-}

module Views.Templates
    ( pageTemplate
    , baseTemplate
    , renderHead
    , renderHeader
    , renderFooter
    ) where

import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Data.Text (Text)

-- Base HTML template structure
baseTemplate :: Text -> Html -> Html
baseTemplate title content = docTypeHtml $ do
    renderHead title
    H.body $ do
        renderHeader
        H.main ! A.class_ "main-content" $ content
        renderFooter

-- Page template with custom content
pageTemplate :: Text -> Html -> Html
pageTemplate pageTitle pageContent = baseTemplate pageTitle $ do
    H.div ! A.class_ "page-content" $ pageContent

-- Head section with all necessary meta tags and stylesheets
renderHead :: Text -> Html
renderHead pageTitle = H.head $ do
    H.meta ! A.charset "UTF-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    H.title (toHtml pageTitle)
    
    -- Stylesheets
    H.link ! A.rel "stylesheet" ! A.href "/css/main.css"
    H.link ! A.rel "stylesheet" ! A.href "/css/components.css"
    H.link ! A.rel "stylesheet" ! A.href "/css/responsive.css"
    
    -- Fonts
    H.link ! A.rel "preconnect" ! A.href "https://fonts.googleapis.com"
    H.link ! A.rel "preconnect" ! A.href "https://fonts.gstatic.com"
    H.link ! A.rel "stylesheet" ! A.href "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&family=Rajdhani:wght@300;400;500;600;700&display=swap"
    
    -- JavaScript
    H.script ! A.src "/js/main.js" $ ""
    H.script ! A.src "/js/background.js" $ ""
    H.script ! A.src "/js/navigation.js" $ ""

-- Header with navigation
renderHeader :: Html
renderHeader = H.header ! A.class_ "main-header" $ do
    H.nav ! A.class_ "main-nav" $ do
        H.div ! A.class_ "nav-container" $ do
            H.div ! A.class_ "nav-brand" $ do
                H.a ! A.href "/" ! A.class_ "brand-logo" $ "PSYOP"
            H.ul ! A.class_ "nav-links" $ do
                H.li $ H.a ! A.href "/" $ "Home"
                H.li $ H.a ! A.href "/music" $ "Music"
                H.li $ H.a ! A.href "/links" $ "Links"
                H.li $ H.a ! A.href "/shows" $ "Shows"
                H.li $ H.a ! A.href "/about" $ "About"
                H.li $ H.a ! A.href "/contact" $ "Contact"

-- Footer section
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
