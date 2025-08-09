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

    it "renders the album art image on home page" $ do
      response <- get "/"
      liftIO $ do
        simpleStatus response `shouldBe` status200
        simpleBody response `shouldSatisfy` 
          \body -> bodyContains "single_spotify_soundcloud_bandcamp.jpg" body &&
                   bodyContains "alt=\"PSYOP Album Art\"" body

    it "contains proper home page content" $ do
      response <- get "/"
      liftIO $ do
        simpleStatus response `shouldBe` status200
        simpleBody response `shouldSatisfy` 
          \body -> bodyContains "PSYOP Album Art" body

  describe "GET /about" $ do
    it "responds with 200" $ do
      get "/about" `shouldRespondWith` 200

    it "contains about page content" $ do
      response <- get "/about"
      liftIO $ do
        simpleStatus response `shouldBe` status200
        simpleBody response `shouldSatisfy` 
          \body -> bodyContains "About PSYOP" body

  describe "GET /static/single_spotify_soundcloud_bandcamp.jpg" $ do
    it "serves static image file" $ do
      get "/static/single_spotify_soundcloud_bandcamp.jpg" `shouldRespondWith` 200
