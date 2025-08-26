{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Network.Wai (Application, defaultRequest)
import Network.Wai.Test (runSession, srequest, SRequest(..), simpleStatus, simpleBody)
import Network.HTTP.Types (status200, status404, methodGet)
import qualified Data.ByteString.Lazy.Char8 as LBS

import App (app, loadFallbackEnv)

mkApp :: IO Application
mkApp = do
  env <- loadFallbackEnv
  pure (app env)

main :: IO ()
main = hspec $ beforeAll mkApp $ do
  describe "Fallback routes" $ do
    it "/, /index, /index.html, /home return 200" $ \a -> runSession (do
      let go p = srequest (SRequest defaultRequest{ Network.Wai.requestMethod = methodGet, Network.Wai.rawPathInfo = p } "") a
      mapM_ (\p -> go p >>= (\res -> simpleStatus res `shouldBe` status200)) ["/", "/index", "/index.html", "/home"]) a

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


