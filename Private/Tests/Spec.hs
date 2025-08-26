module Main (main) where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application, defaultRequest, requestMethod, rawPathInfo)
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
      let go p = srequest (SRequest defaultRequest{ requestMethod = methodGet, rawPathInfo = p } LBS.empty)
      mapM_ (\p -> do
        res <- go p
        liftIO $ simpleStatus res `shouldBe` status200
        liftIO $ LBS.unpack (simpleBody res) `shouldContain` "<style") (fmap B8.pack ["/", "/index", "/index.html", "/home"]) ) a

  describe "Lite routes no-CSS" $ do
    it "/lite.html returns 200 and has no <style>" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ requestMethod = methodGet, rawPathInfo = B8.pack "/lite.html" } LBS.empty)
      liftIO $ simpleStatus res `shouldBe` status200
      let b = LBS.unpack (simpleBody res)
      liftIO $ b `shouldNotContain` "<style"
      liftIO $ b `shouldContain` "<title"
      liftIO $ b `shouldContain` "rel=\"canonical\""
      liftIO $ b `shouldContain` "og:title"
      liftIO $ b `shouldContain` "og:description"
      liftIO $ b `shouldContain` "og:image") a

    it "/lite returns 200 and has no <style>" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ requestMethod = methodGet, rawPathInfo = B8.pack "/lite" } LBS.empty)
      liftIO $ simpleStatus res `shouldBe` status200
      liftIO $ LBS.unpack (simpleBody res) `shouldNotContain` "<style") a

  describe "Production route 404" $ do
    it "/production returns 404" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ requestMethod = methodGet, rawPathInfo = B8.pack "/production" } LBS.empty)
      liftIO $ simpleStatus res `shouldBe` status404) a

  describe "SEO assets" $ do
    it "/robots.txt returns 200 text/plain" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ requestMethod = methodGet, rawPathInfo = B8.pack "/robots.txt" } LBS.empty)
      liftIO $ simpleStatus res `shouldBe` status200
      let ct = lookup (CI.mk (B8.pack "Content-Type")) (simpleHeaders res)
      liftIO $ ct `shouldBe` Just (B8.pack "text/plain")
      liftIO $ LBS.unpack (simpleBody res) `shouldContain` "Sitemap:") a

    it "/sitemap.xml returns 200 application/xml" $ \a -> runSession (do
      res <- srequest (SRequest defaultRequest{ requestMethod = methodGet, rawPathInfo = B8.pack "/sitemap.xml" } LBS.empty)
      liftIO $ simpleStatus res `shouldBe` status200
      let ct = lookup (CI.mk (B8.pack "Content-Type")) (simpleHeaders res)
      liftIO $ ct `shouldBe` Just (B8.pack "application/xml")
      liftIO $ LBS.unpack (simpleBody res) `shouldContain` "<urlset") a


