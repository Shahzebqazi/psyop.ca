{-# LANGUAGE OverloadedStrings #-}

module Components.Pages.Contact where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Components.UI.Cards (contactCard, socialCard)

-- | Contact page view
contactView :: Html
contactView = H.div ! A.class_ "page contact-page" $ do
    H.div ! A.class_ "page-content" $ do
        H.h1 $ "Contact"
        H.p ! A.class_ "page-description" $ "Get in touch with PSYOP"
        
        contactInfoSection
        socialLinksSection

-- | Contact information section
contactInfoSection :: Html
contactInfoSection = H.div ! A.class_ "contact-info" $ do
    contactCard "General Inquiries" "info@psyop.ca" "For all general questions and information"
    contactCard "Booking" "booking@psyop.ca" "For show bookings and event inquiries"
    contactCard "Press & Media" "press@psyop.ca" "For press inquiries and media requests"

-- | Social links section
socialLinksSection :: Html
socialLinksSection = H.div ! A.class_ "social-links-contact" $ do
    H.h2 $ "Follow Us"
    H.div ! A.class_ "social-grid" $ do
        socialCard "https://instagram.com/psyopband" "instagram" "Instagram" "Latest photos and updates"
        socialCard "https://twitter.com/psyopband" "twitter" "Twitter" "News and announcements"
        socialCard "https://facebook.com/psyopband" "facebook" "Facebook" "Connect with our community"
