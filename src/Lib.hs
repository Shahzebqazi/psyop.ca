{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( -- Core types
      SiteSection(..)
    ) where

import Data.Text (Text)

-- Site map types - Home, Links, and Shop
data SiteSection = Home | Links | Shop deriving (Show, Eq, Enum, Bounded)