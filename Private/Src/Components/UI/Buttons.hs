{-# LANGUAGE OverloadedStrings #-}

module Components.UI.Buttons where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T

-- | Primary call-to-action button
ctaButton :: T.Text -> T.Text -> Html
ctaButton href text = H.a ! A.href (toValue href) ! A.class_ "cta-button primary" $ toHtml text

-- | Secondary call-to-action button
ctaButtonSecondary :: T.Text -> T.Text -> Html
ctaButtonSecondary href text = H.a ! A.href (toValue href) ! A.class_ "cta-button secondary" $ toHtml text

-- | Generic button component
button :: T.Text -> T.Text -> Html
button href text = H.a ! A.href (toValue href) ! A.class_ "button" $ toHtml text

-- | Social media button
socialButton :: T.Text -> T.Text -> T.Text -> Html
socialButton href platform text = H.a ! A.href (toValue href) ! A.target "_blank" ! A.class_ (toValue ("social-button " <> platform)) $ toHtml text

-- | Platform link button (for streaming services)
platformButton :: T.Text -> T.Text -> T.Text -> Html
platformButton href platform text = H.a ! A.href (toValue href) ! A.target "_blank" ! A.class_ (toValue ("platform-button " <> platform)) $ toHtml text
