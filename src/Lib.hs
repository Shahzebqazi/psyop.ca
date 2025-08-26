{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( -- Core types
      SiteSection(..)
    , sectionToTitle
    ) where

import Data.Text (Text)

-- Site map types - Home, Links, and Shop
data SiteSection = Home | Links | Shop deriving (Show, Eq, Enum, Bounded)

-- Convert section to a human-friendly title
sectionToTitle :: SiteSection -> String
sectionToTitle s = case s of
    Home  -> "Home"
    Links -> "Links"
    Shop  -> "Shop"