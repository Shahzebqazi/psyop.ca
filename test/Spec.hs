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
import Lib (app, ImageSequence(..), mkImageSequence, mkSequenceSize, nextImage, getCurrentImage, testNoRepetition, ImageIndex(..), SequenceSize(..))

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

      describe "GET /health" $ do
        it "responds with 200" $ do
          get "/health" `shouldRespondWith` 200

        it "contains health check information" $ do
          response <- get "/health"
          liftIO $ do
            simpleStatus response `shouldBe` status200
            simpleBody response `shouldSatisfy` 
              \body -> bodyContains "System Health" body &&
                       bodyContains "All Systems Operational" body &&
                       bodyContains "Configuration" body

  describe "Image Sequence Model Tests" $ do
    it "creates sequence with correct number of items" $ do
      let Just size10 = mkSequenceSize 10
          Just size50 = mkSequenceSize 50
          Just size100 = mkSequenceSize 100
          seq10 = mkImageSequence size10
          seq50 = mkImageSequence size50
          seq100 = mkImageSequence size100
      totalImages seq10 `shouldBe` SequenceSize 10
      totalImages seq50 `shouldBe` SequenceSize 50
      totalImages seq100 `shouldBe` SequenceSize 100

    it "rejects invalid sequence sizes" $ do
      mkSequenceSize 0 `shouldBe` Nothing
      mkSequenceSize (-1) `shouldBe` Nothing
      mkSequenceSize 1001 `shouldBe` Nothing

    it "nextImage advances sequence correctly" $ do
      let Just size = mkSequenceSize 5
          seq = mkImageSequence size
          nextSeq = nextImage seq
      currentIndex seq `shouldBe` ImageIndex 0
      currentIndex nextSeq `shouldBe` ImageIndex 1
      usedCount nextSeq `shouldBe` 1

    it "ensures no repetition within sequence length" $ do
      testNoRepetition 10 `shouldBe` True
      testNoRepetition 50 `shouldBe` True
      testNoRepetition 100 `shouldBe` True

    it "getCurrentImage returns correct index" $ do
      let Just size = mkSequenceSize 7
          seq = mkImageSequence size
          currentIdx = getCurrentImage seq
      currentIdx `shouldBe` ImageIndex 0
