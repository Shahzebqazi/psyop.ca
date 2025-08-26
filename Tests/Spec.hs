{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Network.Wai (Application, Request, Response, defaultRequest)
import Network.Wai.Test (Session, request, runSession, simpleBody, simpleHeaders, simpleStatus, srequest, SRequest(..))
import Network.HTTP.Types (status200, status404, methodGet)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B8

import App (app, loadFallbackEnv)

mkApp :: IO Application
mkApp = do
  env <- loadFallbackEnv
  pure (app env)

main :: IO ()
main = hspec $ beforeAll mkApp $ do
  describe "Fallback routes with CSS" $ do
    it "/ returns 200" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet } "") a
      simpleStatus res `shouldBe` status200) a

    it "/index returns 200" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/index" } "") a
      simpleStatus res `shouldBe` status200) a

    it "/index.html returns 200" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/index.html" } "") a
      simpleStatus res `shouldBe` status200) a

    it "/home returns 200" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/home" } "") a
      simpleStatus res `shouldBe` status200) a

  describe "Lite routes without CSS" $ do
    it "/lite.html returns 200 and contains no <style> tag" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/lite.html" } "") a
      simpleStatus res `shouldBe` status200
      let body = simpleBody res
      LBS.unpack body `shouldNotContain` "<style"
      LBS.unpack body `shouldContain` "<title"
      LBS.unpack body `shouldContain` "rel=\"canonical\""
      LBS.unpack body `shouldContain` "og:title"
      LBS.unpack body `shouldContain` "og:description"
      LBS.unpack body `shouldContain` "og:image") a

    it "/lite returns 200 and contains no <style> tag" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/lite" } "") a
      simpleStatus res `shouldBe` status200
      LBS.unpack (simpleBody res) `shouldNotContain` "<style") a

  describe "Production route is 404" $ do
    it "/production returns 404" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/production" } "") a
      simpleStatus res `shouldBe` status404) a


