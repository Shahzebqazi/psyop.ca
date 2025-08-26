{-# LANGUAGE OverloadedStrings #-}

module Components.UI.Cards where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T

-- | Platform card for streaming services
platformCard :: T.Text -> T.Text -> T.Text -> T.Text -> Html
platformCard platform title description linkText = H.div ! A.class_ "platform-card" $ do
    H.h3 $ toHtml title
    H.p $ toHtml description
    H.a ! A.href (toValue platform) ! A.target "_blank" ! A.class_ "platform-link" $ toHtml linkText

-- | Social media card
socialCard :: T.Text -> T.Text -> T.Text -> T.Text -> Html
socialCard href platform title description = H.a ! A.href (toValue href) ! A.target "_blank" ! A.class_ (toValue ("social-card " <> platform)) $ do
    H.h3 $ toHtml title
    H.p $ toHtml description

-- | Tour date card
tourCard :: T.Text -> T.Text -> T.Text -> Html
tourCard date location venue = H.div ! A.class_ "tour-card" $ do
    H.div ! A.class_ "tour-date" $ toHtml date
    H.div ! A.class_ "tour-location" $ toHtml location
    H.div ! A.class_ "tour-venue" $ toHtml venue

-- | Contact info card
contactCard :: T.Text -> T.Text -> T.Text -> Html
contactCard title email description = H.div ! A.class_ "contact-card" $ do
    H.h3 $ toHtml title
    H.p $ H.a ! A.href (toValue ("mailto:" <> email)) $ toHtml email
    H.p $ toHtml description
