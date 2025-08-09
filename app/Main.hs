module Main (main) where

import Lib

main :: IO ()
main = do
    putStrLn "Starting PSYOP website on http://localhost:8080"
    startApp
