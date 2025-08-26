{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- Import all our new components
import Components.Layout (mainLayout)
import Components.Pages.Home (homeView)
import Components.Pages.Music (musicView)
import Components.Pages.Tour (tourView)
import Components.Pages.Contact (contactView)
import Components.Pages.Legal (privacyView, termsView)

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

-- | Render home page with layout
renderHomePage :: String
renderHomePage = renderView $ mainLayout "Home" homeView

-- | Render music page with layout
renderMusicPage :: String
renderMusicPage = renderView $ mainLayout "Music" musicView

-- | Render tour page with layout
renderTourPage :: String
renderTourPage = renderView $ mainLayout "Tour" tourView

-- | Render contact page with layout
renderContactPage :: String
renderContactPage = renderView $ mainLayout "Contact" contactView

-- | Render privacy page with layout
renderPrivacyPage :: String
renderPrivacyPage = renderView $ mainLayout "Privacy Policy" privacyView

-- | Render terms page with layout
renderTermsPage :: String
renderTermsPage = renderView $ mainLayout "Terms of Service" termsView

-- | Render error page with layout
renderErrorPage :: String
renderErrorPage = renderView $ mainLayout "Page Not Found" errorView
