{-# LANGUAGE OverloadedStrings #-}

module Components.Pages.Legal where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T

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
