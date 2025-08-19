{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Wai
import Network.Wai (Application)
import Network.HTTP.Types
import Network.Wai.Test (simpleStatus, simpleBody)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (isInfixOf)
import Lib (app, ImageSequence(..), createImageSequence, nextImage, getCurrentImage, testNoRepetition)

-- Helper function to check if a string is contained in lazy bytestring
bodyContains :: String -> LBS.ByteString -> Bool
bodyContains needle haystack = needle `isInfixOf` L8.unpack haystack

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Web API Tests" $ do
    with (return app) $ do
      describe "GET /" $ do
        it "responds with 200" $ do
          get "/" `shouldRespondWith` 200

        it "renders the logo on home page" $ do
          response <- get "/"
          liftIO $ do
            simpleStatus response `shouldBe` status200
            simpleBody response `shouldSatisfy` 
              \body -> bodyContains "PSYOP" body &&
                       bodyContains "main-logo" body

        it "contains proper home page content" $ do
          response <- get "/"
          liftIO $ do
            simpleStatus response `shouldBe` status200
            simpleBody response `shouldSatisfy` 
              \body -> bodyContains "MOONLIGHT PARADOX" body &&
                       bodyContains "NEW TRACK" body

        it "contains navigation menu" $ do
          response <- get "/"
          liftIO $ do
            simpleStatus response `shouldBe` status200
            simpleBody response `shouldSatisfy` 
              \body -> bodyContains "HOME" body &&
                       bodyContains "ABOUT" body &&
                       bodyContains "SOCIALS" body

      describe "GET /about" $ do
        it "responds with 200" $ do
          get "/about" `shouldRespondWith` 200

        it "contains about page content" $ do
          response <- get "/about"
          liftIO $ do
            simpleStatus response `shouldBe` status200
            simpleBody response `shouldSatisfy` 
              \body -> bodyContains "About PSYOP" body

      describe "GET /css/style.css" $ do
        it "serves CSS file" $ do
          get "/css/style.css" `shouldRespondWith` 200

      describe "GET /contact" $ do
        it "responds with 200" $ do
          get "/contact" `shouldRespondWith` 200

        it "contains contact information" $ do
          response <- get "/contact"
          liftIO $ do
            simpleStatus response `shouldBe` status200
            simpleBody response `shouldSatisfy` 
              \body -> bodyContains "admin@psyop.ca" body

      describe "GET /links" $ do
        it "responds with 200" $ do
          get "/links" `shouldRespondWith` 200

      describe "GET /admin" $ do
        it "responds with 200" $ do
          get "/admin" `shouldRespondWith` 200

      describe "GET /test" $ do
        it "responds with 200" $ do
          get "/test" `shouldRespondWith` 200

        it "contains image sequence test results" $ do
          response <- get "/test"
          liftIO $ do
            simpleStatus response `shouldBe` status200
            simpleBody response `shouldSatisfy` 
              \body -> bodyContains "Image Sequence Model Tests" body &&
                       bodyContains "10 items" body &&
                       bodyContains "50 items" body &&
                       bodyContains "100 items" body

  describe "Image Sequence Model Tests" $ do
    it "creates sequence with correct number of items" $ do
      let seq10 = createImageSequence 10
          seq50 = createImageSequence 50
          seq100 = createImageSequence 100
      totalImages seq10 `shouldBe` 10
      totalImages seq50 `shouldBe` 50
      totalImages seq100 `shouldBe` 100

    it "nextImage advances sequence correctly" $ do
      let seq = createImageSequence 5
          nextSeq = nextImage seq
      currentIndex seq `shouldBe` 0
      currentIndex nextSeq `shouldBe` 1
      length (usedIndices nextSeq) `shouldBe` 1

    it "ensures no repetition within sequence length" $ do
      testNoRepetition 10 `shouldBe` True
      testNoRepetition 50 `shouldBe` True
      testNoRepetition 100 `shouldBe` True

    it "getCurrentImage returns correct index" $ do
      let seq = createImageSequence 7
          currentIdx = getCurrentImage seq
      currentIdx `shouldBe` 0
