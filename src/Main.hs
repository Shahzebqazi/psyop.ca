{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Handler.Warp (run, defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import System.Environment (lookupEnv)
import Data.Char (toLower)
import Text.Read (readMaybe)
import Models (defaultConfig, getServerPort)
import App (app, loadFallbackEnv, FallbackEnv)

-- Main entry point for MenuBar testing
main :: IO ()
main = do
    putStrLn "🚀 Starting PSYOP Server..."
    putStrLn $ "🌐 Starting fallback server on port " ++ show (getServerPort defaultConfig)
    env <- loadFallbackEnv
    startServer env

-- Start the server
startServer :: FallbackEnv -> IO ()
startServer env = do
    mPortEnv <- lookupEnv "PORT"
    let port = case mPortEnv >>= readMaybe of
            Just p  -> p
            Nothing -> getServerPort defaultConfig
    httpsEnable <- lookupEnv "HTTPS_ENABLE"
    certFile    <- lookupEnv "CERT_FILE"
    keyFile     <- lookupEnv "KEY_FILE"
    case fmap (map toLower) httpsEnable of
        Just "true" -> case (certFile, keyFile) of
            (Just cert, Just key) -> do
                putStrLn $ "🔐 Starting HTTPS server on port " ++ show port
                putStrLn $ "    cert: " ++ cert
                putStrLn $ "    key : " ++ key
                putStrLn $ "🧭 Visit https://localhost:" ++ show port ++ "/index.html"
                runTLS (tlsSettings cert key) (setPort port defaultSettings) (app env)
            _ -> do
                putStrLn "⚠️ HTTPS_ENABLE=true but CERT_FILE or KEY_FILE not set; starting HTTP instead"
                putStrLn $ "🌐 Server running at http://localhost:" ++ show port
                putStrLn $ "🧭 Visit http://localhost:" ++ show port ++ "/index.html"
                run port (app env)
        _ -> do
            putStrLn $ "🌐 Server running at http://localhost:" ++ show port
            putStrLn $ "🧭 Visit http://localhost:" ++ show port ++ "/index.html"
            run port (app env)


