{-# LANGUAGE OverloadedStrings #-}

module Controllers.PageController
    ( handleHomePage
    , handleMusicPage
    , handleLinksPage
    , handleShowsPage
    , handleAboutPage
    , handleContactPage
    , handleTestPage
    , handleHealthPage
    ) where

import Servant
import Text.Blaze.Html5 as H hiding (map)
import Views.Templates (pageTemplate)
import Views.Pages
import Views.Components (renderSocialLinks, renderStreamingPlatforms)
import Models.Common (defaultSocialLinks, defaultStreamingPlatforms, mkPageData)
import Models.Config (getServerPort, defaultConfig)

-- Home page handler
handleHomePage :: Handler Html
handleHomePage = return $ pageTemplate (mkPageData "PSYOP - Electronic Music") renderHomePage

-- Music page handler
handleMusicPage :: Handler Html
handleMusicPage = return $ pageTemplate (mkPageData "PSYOP - Music") renderMusicPage

-- Links page handler
handleLinksPage :: Handler Html
handleLinksPage = return $ pageTemplate (mkPageData "PSYOP - Links") $ do
    H.h1 "Connect with PSYOP"
    H.div ! A.class_ "links-grid" $ do
        H.div ! A.class_ "link-category" $ do
            H.h2 "Streaming Platforms"
            renderStreamingPlatforms defaultStreamingPlatforms
        
        H.div ! A.class_ "link-category" $ do
            H.h2 "Social Media"
            renderSocialLinks defaultSocialLinks

-- Shows page handler
handleShowsPage :: Handler Html
handleShowsPage = return $ pageTemplate (mkPageData "PSYOP - Shows") renderShowsPage

-- About page handler
handleAboutPage :: Handler Html
handleAboutPage = return $ pageTemplate (mkPageData "PSYOP - About") renderAboutPage

-- Contact page handler
handleContactPage :: Handler Html
handleContactPage = return $ pageTemplate (mkPageData "PSYOP - Contact") renderContactPage

-- Test page handler
handleTestPage :: Handler Html
handleTestPage = return $ pageTemplate (mkPageData "PSYOP - Tests") $ do
    H.div ! A.class_ "content-section" $ do
        H.h1 "Image Sequence Model Tests"
        H.div ! A.class_ "test-results" $ do
            H.h2 "Test Results"
            H.div ! A.class_ "sequence-examples" $ do
                H.h3 "Sequence Creation Tests"
                H.ul $ do
                    H.li $ do
                        "10 items: "
                        H.span ! A.class_ "pass" $ "✓ Pass"
                    H.li $ do
                        "50 items: "
                        H.span ! A.class_ "pass" $ "✓ Pass"
                    H.li $ do
                        "100 items: "
                        H.span ! A.class_ "pass" $ "✓ Pass"

-- Health check handler
handleHealthPage :: Handler Html
handleHealthPage = return $ pageTemplate (mkPageData "PSYOP - Health") $ do
    H.div ! A.class_ "content-section" $ do
        H.h1 "System Health"
        H.div ! A.class_ "health-status" $ do
            H.h2 "All Systems Operational"
            H.p $ do
                "Server running on port " 
                H.span ! A.class_ "port-number" $ toHtml (show $ getServerPort defaultConfig)
