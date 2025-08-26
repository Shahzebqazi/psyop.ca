{-# LANGUAGE OverloadedStrings #-}

module Components.Pages.Home where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Components.UI.Buttons (ctaButton, ctaButtonSecondary)

-- | Home page view with bio
homeView :: Html
homeView = H.div ! A.class_ "page home-page" $ do
    H.div ! A.class_ "page-content" $ do
        heroSection
        bioSection
        ctaSection

-- | Hero section with title and subtitle
heroSection :: Html
heroSection = H.div ! A.class_ "hero-section" $ do
    H.h1 ! A.class_ "hero-title" $ "PSYOP"
    H.p ! A.class_ "hero-subtitle" $ "ˈsaɪ.ɑp"
    H.p ! A.class_ "hero-description" $ "4‑piece Metal Machine from Toronto, Canada"

-- | Bio section with band information
bioSection :: Html
bioSection = H.div ! A.class_ "bio-section" $ do
    H.h2 $ "About"
    H.div ! A.class_ "bio-content" $ do
        H.p $ "PSYOP is a dynamic metal band pushing the boundaries of heavy music. With influences ranging from classic metal to modern progressive sounds, we create an intense and immersive musical experience."
        H.p $ "Our sound combines crushing riffs, technical precision, and atmospheric elements to deliver a unique take on contemporary metal."
        H.p $ "Based in Toronto, we're actively performing and recording new material. Stay tuned for upcoming releases and tour dates."

-- | Call-to-action section
ctaSection :: Html
ctaSection = H.div ! A.class_ "cta-section" $ do
    H.h3 $ "Listen Now"
    H.div ! A.class_ "cta-buttons" $ do
        ctaButton "/music" "Stream Music"
        ctaButtonSecondary "/tour" "See Tour Dates"
