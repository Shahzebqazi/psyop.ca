{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Handler.Warp
import Models (defaultConfig, getServerPort)
import App (app, loadFallbackEnv, FallbackEnv)

-- Main entry point for MenuBar testing
main :: IO ()
main = do
    putStrLn "🚀 Starting PSYOP Server..."
    putStrLn $ "🌐 Starting server on port " ++ show (getServerPort defaultConfig)
    env <- loadFallbackEnv
    startServer env

-- Start the server
startServer :: FallbackEnv -> IO ()
startServer env = do
    let port = getServerPort defaultConfig
    putStrLn $ "🌐 Server running at http://localhost:" ++ show port
    putStrLn $ "🧭 Visit http://localhost:" ++ show port ++ "/index.html for fallback"
    run port (app env)


