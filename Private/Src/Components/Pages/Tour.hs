{-# LANGUAGE OverloadedStrings #-}

module Components.Pages.Tour where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Components.UI.Cards (tourCard)
import Components.UI.Buttons (ctaButton)

-- | Tour page view with dates
tourView :: Html
tourView = H.div ! A.class_ "page tour-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.h1 $ "Tour"
        H.p ! A.class_ "page-description" $ "See PSYOP live"
        
        tourDatesSection
        tourCtaSection

-- | Tour dates section
tourDatesSection :: Html
tourDatesSection = H.div ! A.class_ "tour-dates" $ do
    H.h2 $ "Upcoming Shows"
    H.div ! A.class_ "tour-list" $ do
        tourCard "Coming Soon" "More dates to be announced" "Stay tuned for updates"

-- | Tour call-to-action section
tourCtaSection :: Html
tourCtaSection = H.div ! A.class_ "tour-cta" $ do
    H.p $ "Want us to play your venue? Get in touch!"
    ctaButton "/contact" "Contact Us"
