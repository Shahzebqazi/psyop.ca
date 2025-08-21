{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Lib (SiteSection(..), sectionToTitle)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Domain Model Validation Tests" $ do
    it "validates site sections" $ do
      let expectedSections = [Home, Links, Shop]
      let sectionTitles = map sectionToTitle expectedSections
      let expectedTitles = ["Home", "Links", "Shop"]
      sectionTitles `shouldBe` expectedTitles

  describe "Business Logic Tests" $ do
    it "has correct site sections" $ do
      let sections = [Home, Links, Shop]
      length sections `shouldBe` 3
      sectionToTitle Home `shouldBe` "Home"
      sectionToTitle Links `shouldBe` "Links"
      sectionToTitle Shop `shouldBe` "Shop"
