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
import Lib (app)

-- Helper function to check if a string is contained in lazy bytestring
bodyContains :: String -> LBS.ByteString -> Bool
bodyContains needle haystack = needle `isInfixOf` L8.unpack haystack

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "renders the logo on home page" $ do
      response <- get "/"
      liftIO $ do
        simpleStatus response `shouldBe` status200
        simpleBody response `shouldSatisfy` 
          \body -> bodyContains "psyop-logo.png" body &&
                   bodyContains "alt=\"PSYOP Logo\"" body

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

  describe "GET /static/psyop-logo.png" $ do
    it "serves static logo file" $ do
      get "/static/psyop-logo.png" `shouldRespondWith` 200

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
