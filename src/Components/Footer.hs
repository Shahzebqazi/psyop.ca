{-# LANGUAGE OverloadedStrings #-}

module Components.Footer
    ( Footer(..)
    , mkFooter
    , renderFooter
    ) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

-- Minimal footer model for fallback site
data Footer = Footer deriving (Show, Eq)

mkFooter :: Footer
mkFooter = Footer

renderFooter :: Footer -> Html
renderFooter _ =
    H.footer ! A.class_ "main-footer" $ do
        H.div ! A.class_ "footer-content" $ do
            H.div ! A.class_ "footer-main" $ do
                H.div ! A.class_ "footer-brand" $ "PSYOP"
                H.p ! A.class_ "footer-description" $ "Toronto Metal Machine"
            H.div ! A.class_ "footer-bottom" $ do
                H.div ! A.class_ "footer-copyright" $ "© 2025 PSYOP"
                H.div ! A.class_ "footer-legal" $ do
                    H.a ! A.href "#home" $ "Home"
                    H.span ! A.class_ "separator" $ "|"
                    H.a ! A.href "#links" $ "Links"


