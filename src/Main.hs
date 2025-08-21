{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Handler.Warp
import Models (defaultConfig, getServerPort)
import App (app)

-- Main entry point for MenuBar testing
main :: IO ()
main = do
    putStrLn "🚀 Starting PSYOP MenuBar Test Server..."
    putStrLn $ "🌐 Starting server on port " ++ show (getServerPort defaultConfig)
    putStrLn "🧭 Testing MenuBar component only"
    startServer

-- Start the server
startServer :: IO ()
startServer = do
    let port = getServerPort defaultConfig
    putStrLn $ "🌐 Server running at http://localhost:" ++ show port
    putStrLn $ "🧭 Visit http://localhost:" ++ show port ++ " to test MenuBar"
    run port app


