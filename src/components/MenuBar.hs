{-# LANGUAGE OverloadedStrings #-}

module Components.MenuBar
    ( MenuBar(..)
    , renderMenuBar
    , mkMenuBar
    , updateMenuState
    , toggleMobileMenu
    ) where

import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Data.Text (Text)
import Lib (SiteSection(..))

-- Menu bar component state
data MenuBar = MenuBar
    { currentSection :: SiteSection
    , isMenuOpen :: Bool  -- For mobile menu
    } deriving (Show, Eq)

-- Create a new menu bar
mkMenuBar :: SiteSection -> MenuBar
mkMenuBar menuSection = MenuBar menuSection False

-- Render the menu bar component
renderMenuBar :: MenuBar -> Html
renderMenuBar menuBar = 
    H.header ! A.id "main-menu" ! A.class_ "main-header" $ do
        H.nav ! A.class_ "main-nav" $ do
            H.div ! A.class_ "nav-container" $ do
                renderBrandLogo
                renderMobileToggle menuBar
                renderNavigationLinks menuBar

-- Render the brand logo with enhanced styling
renderBrandLogo :: Html
renderBrandLogo = 
    H.a ! A.href "/" ! A.class_ "brand-logo" $ do
        H.span ! A.class_ "brand-text" $ "PSYOP"
        H.span ! A.class_ "brand-subtitle" ! A.id "scrolling-subtitle" $ "心理戦"
        H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml $ 
            "const psyopDefinition = 'Psychologische Operation - Eine militärische Operation, die darauf abzielt, die Emotionen, Einstellungen und das Verhalten von Zielgruppen zu beeinflussen, um nationale Ziele zu unterstützen.';" ++
            "let currentIndex = 0;" ++
            "function updateScrollingSubtitle() {" ++
            "    const subtitle = document.getElementById('scrolling-subtitle');" ++
            "    if (subtitle) {" ++
            "        const text = psyopDefinition + psyopDefinition;" ++
            "        const displayText = text.substr(currentIndex, 8);" ++
            "        subtitle.textContent = displayText;" ++
            "        currentIndex = (currentIndex + 1) % psyopDefinition.length;" ++
            "    }" ++
            "}" ++
            "setInterval(updateScrollingSubtitle, 200);"

-- Render mobile menu toggle button
renderMobileToggle :: MenuBar -> Html
renderMobileToggle menuBar = 
    H.button ! A.class_ "mobile-menu-toggle"
              ! A.type_ "button"
              ! A.id "mobile-toggle"
              ! A.onclick "toggleMobileMenu()" $ do
        H.span ! A.class_ "hamburger-line" $ ""
        H.span ! A.class_ "hamburger-line" $ ""
        H.span ! A.class_ "hamburger-line" $ ""

-- Render the navigation links with mobile support
renderNavigationLinks :: MenuBar -> Html
renderNavigationLinks menuBar = 
    H.div ! A.class_ "nav-section" $ do
        H.ul ! A.class_ (toValue $ getNavLinksClass menuBar) $ do
            mapM_ (renderMenuItem menuBar) [Home, Links] -- Shop commented out for future implementation
        renderCartIcon

-- Get CSS class for navigation links (including mobile state)
getNavLinksClass :: MenuBar -> Text
getNavLinksClass menuBar = 
    let baseClass = "nav-links"
        mobileClass = if isMenuOpen menuBar then " mobile-open" else ""
    in baseClass <> mobileClass

-- Render individual menu item with enhanced styling
renderMenuItem :: MenuBar -> SiteSection -> Html
renderMenuItem menuBar menuSection = 
    H.li ! A.class_ "nav-item" $ 
        H.a ! A.href (toValue $ getMenuItemUrl menuSection)
           ! A.class_ (toValue $ getMenuItemClass menuBar menuSection)
           ! A.onclick (toValue $ getMenuItemOnClick menuSection)
           $ toHtml $ sectionToTitle menuSection

-- Get URL for menu item (now using anchor links)
getMenuItemUrl :: SiteSection -> Text
getMenuItemUrl menuSection = case menuSection of
    Home -> "#home"
    Links -> "#links"
            -- Shop -> "#shop" -- Commented out for future implementation

-- Get CSS class for menu item (including active state)
getMenuItemClass :: MenuBar -> SiteSection -> Text
getMenuItemClass menuBar menuSection = 
    let baseClass = "nav-link"
        activeClass = if currentSection menuBar == menuSection then " active" else ""
    in baseClass <> activeClass

-- Get onClick handler for menu items (smooth scrolling)
getMenuItemOnClick :: SiteSection -> Text
getMenuItemOnClick menuSection = case menuSection of
    Home -> "scrollToSection('home'); return false;"
    Links -> "scrollToSection('links'); return false;"
            -- Shop -> "scrollToSection('shop'); return false;" -- Commented out for future implementation

-- Convert section to display title
sectionToTitle :: SiteSection -> Text
sectionToTitle menuSection = case menuSection of
    Home -> "Home"
    Links -> "Links"
            -- Shop -> "Shop" -- Commented out for future implementation

-- Convert section to ID string
sectionToId :: SiteSection -> Text
sectionToId menuSection = case menuSection of
    Home -> "home"
    Links -> "links"
            -- Shop -> "shop" -- Commented out for future implementation

-- Update menu state (for future interactive features)
updateMenuState :: MenuBar -> SiteSection -> MenuBar
updateMenuState menuBar newSection = menuBar { currentSection = newSection }

-- Toggle mobile menu
toggleMobileMenu :: MenuBar -> MenuBar
toggleMobileMenu menuBar = menuBar { isMenuOpen = not (isMenuOpen menuBar) }

-- Render cart icon with shopping cart functionality
renderCartIcon :: Html
renderCartIcon = 
    H.div ! A.class_ "cart-icon-container" $ do
        H.a ! A.href "#" ! A.class_ "cart-icon" ! A.id "cart-icon" ! A.onclick "toggleCartDropdown(); return false;" $ do
            H.span ! A.class_ "cart-icon-symbol" $ "CART"
        H.div ! A.class_ "cart-dropdown" ! A.id "cart-dropdown" $ do
            H.div ! A.class_ "cart-dropdown-content" $ do
                H.div ! A.class_ "cart-empty-message" $ "CART EMPTY"
                H.button ! A.class_ "cart-close-btn" ! A.onclick "toggleCartDropdown()" $ "Close"
        H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml $ 
            "// Cart functionality\n" ++
            "let cartItemCount = 0;\n" ++
            "let isCartOpen = false;\n" ++
            "const cartIcon = document.getElementById('cart-icon');\n" ++
            "const cartDropdown = document.getElementById('cart-dropdown');\n" ++
            "\n" ++
            "function updateCartIcon() {\n" ++
            "    if (cartItemCount > 0) {\n" ++
            "        cartIcon.classList.add('has-items');\n" ++
            "    } else {\n" ++
            "        cartIcon.classList.remove('has-items');\n" ++
            "    }\n" ++
            "}\n" ++
            "\n" ++
            "function toggleCartDropdown() {\n" ++
            "    isCartOpen = !isCartOpen;\n" ++
            "    if (isCartOpen) {\n" ++
            "        cartDropdown.classList.add('open');\n" ++
            "    } else {\n" ++
            "        cartDropdown.classList.remove('open');\n" ++
            "    }\n" ++
            "}\n" ++
            "\n" ++
            "function addToCart() {\n" ++
            "    cartItemCount++;\n" ++
            "    updateCartIcon();\n" ++
            "    // Add a subtle animation\n" ++
            "    cartIcon.style.transform = 'scale(1.1)';\n" ++
            "    setTimeout(() => {\n" ++
            "        cartIcon.style.transform = '';\n" ++
            "    }, 200);\n" ++
            "}\n" ++
            "\n" ++
            "function removeFromCart() {\n" ++
            "    if (cartItemCount > 0) {\n" ++
            "        cartItemCount--;\n" ++
            "        updateCartIcon();\n" ++
            "    }\n" ++
            "}\n" ++
            "\n" ++
            "function clearCart() {\n" ++
            "    cartItemCount = 0;\n" ++
            "    updateCartIcon();\n" ++
            "}\n" ++
            "\n" ++
            "// Close cart dropdown when clicking outside\n" ++
            "document.addEventListener('click', function(event) {\n" ++
            "    if (!event.target.closest('.cart-icon-container') && isCartOpen) {\n" ++
            "        toggleCartDropdown();\n" ++
            "    }\n" ++
            "});\n" ++
            "\n" ++
            "// Initialize cart state\n" ++
            "updateCartIcon();\n" ++
            "\n" ++
            "// Make functions globally available for testing\n" ++
            "window.addToCart = addToCart;\n" ++
            "window.removeFromCart = removeFromCart;\n" ++
            "window.clearCart = clearCart;\n" ++
            "window.toggleCartDropdown = toggleCartDropdown;\n" ++
            "window.cartItemCount = () => cartItemCount;"
