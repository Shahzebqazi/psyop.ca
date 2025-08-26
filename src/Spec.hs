{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Network.Wai (Application, defaultRequest)
import Network.Wai.Test (runSession, srequest, SRequest(..), simpleStatus, simpleBody, simpleHeaders)
import Network.HTTP.Types (status200, status404, methodGet)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LBS

import App (app, loadFallbackEnv)

mkApp :: IO Application
mkApp = do
  env <- loadFallbackEnv
  pure (app env)

main :: IO ()
main = hspec $ beforeAll mkApp $ do
  describe "Fallback routes" $ do
    it "/, /index, /index.html, /home return 200 and contain <style>" $ \a -> runSession (do
      let go p = srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = p } "") a
      mapM_ (\p -> do
        res <- go p
        simpleStatus res `shouldBe` status200
        LBS.unpack (simpleBody res) `shouldContain` "<style") ["/", "/index", "/index.html", "/home"]) a

  describe "Lite routes no-CSS" $ do
    it "/lite.html returns 200 and has no <style>" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/lite.html" } "") a
      simpleStatus res `shouldBe` status200
      let b = LBS.unpack (simpleBody res)
      b `shouldNotContain` "<style"
      b `shouldContain` "<title"
      b `shouldContain` "rel=\"canonical\""
      b `shouldContain` "og:title"
      b `shouldContain` "og:description"
      b `shouldContain` "og:image") a

    it "/lite returns 200 and has no <style>" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/lite" } "") a
      simpleStatus res `shouldBe` status200
      LBS.unpack (simpleBody res) `shouldNotContain` "<style") a

  describe "Production route 404" $ do
    it "/production returns 404" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/production" } "") a
      simpleStatus res `shouldBe` status404) a

  describe "SEO assets" $ do
    it "/robots.txt returns 200 text/plain" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/robots.txt" } "") a
      simpleStatus res `shouldBe` status200
      let ct = lookup (CI.mk (B8.pack "Content-Type")) (simpleHeaders res)
      ct `shouldBe` Just "text/plain"
      LBS.unpack (simpleBody res) `shouldContain` "Sitemap:") a

    it "/sitemap.xml returns 200 application/xml" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = "/sitemap.xml" } "") a
      simpleStatus res `shouldBe` status200
      let ct = lookup (CI.mk (B8.pack "Content-Type")) (simpleHeaders res)
      ct `shouldBe` Just "application/xml"
      LBS.unpack (simpleBody res) `shouldContain` "<urlset") a


