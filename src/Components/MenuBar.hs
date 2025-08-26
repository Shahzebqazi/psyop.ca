{-# LANGUAGE OverloadedStrings #-}

module Components.MenuBar
    ( MenuBar(..)
    , mkMenuBar
    , renderMenuBar
    ) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
-- Fallback-only: no SiteSection required

-- Minimal menubar model for fallback site
data MenuBar = MenuBar
    { activeSection :: String
    } deriving (Show, Eq)

-- Smart constructor
mkMenuBar :: String -> MenuBar
mkMenuBar section = MenuBar { activeSection = section }

-- Render a simple, accessible header/nav compatible with existing CSS/JS
renderMenuBar :: MenuBar -> Html
renderMenuBar _ =
    H.header ! A.class_ "main-header" $ do
        H.nav ! A.class_ "main-nav" $ do
            H.div ! A.class_ "nav-container" $ do
                -- Brand
                H.a ! A.href "#home" ! A.class_ "brand-logo" $ do
                    H.span ! A.class_ "brand-text" $ "PSYOP"
                    H.span ! A.class_ "brand-subtitle" $ "METAL MACHINE"

                -- Mobile menu toggle expected by JS
                H.button ! A.class_ "mobile-menu-toggle" ! A.type_ "button" ! A.onclick "toggleMobileMenu()" $ do
                    H.span ! A.class_ "hamburger-line" $ mempty
                    H.span ! A.class_ "hamburger-line" $ mempty
                    H.span ! A.class_ "hamburger-line" $ mempty

                -- Primary nav
                H.ul ! A.class_ "nav-links" $ do
                    H.li ! A.class_ "nav-item" $ do
                        H.a ! A.href "#home" ! A.class_ "nav-link" $ "Home"
                    H.li ! A.class_ "nav-item" $ do
                        H.a ! A.href "#links" ! A.class_ "nav-link" $ "Links"
                    -- Shop is intentionally omitted in fallback


