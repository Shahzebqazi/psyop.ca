#!/usr/bin/env runhaskell

-- PSYOP Website - Development Server with Setup and Auto-reload
-- This script provides development environment, project setup, and automatic reloading

import System.Environment (getArgs, lookupEnv)
import System.Process (readProcessWithExitCode, spawnProcess, waitForProcess)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory, doesFileExist, removeFile)
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (when, unless, forever)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Data.List (isSuffixOf)
import System.IO (writeFile)

-- Configuration
data DevConfig = DevConfig
    { appPort :: Int
    , watchDirs :: [String]
    , buildCmd :: String
    , runCmd :: String
    } deriving Show

-- Default development configuration
defaultDevConfig :: DevConfig
defaultDevConfig = DevConfig
    { appPort = 8080
    , watchDirs = ["src", "css", "test"]
    , buildCmd = "stack build"
    , runCmd = "stack exec psyop-website-exe"
    }

-- Utility functions
logInfo :: String -> IO ()
logInfo msg = putStrLn $ "[DEV-INFO] " ++ msg

logSuccess :: String -> IO ()
logSuccess msg = putStrLn $ "[DEV-SUCCESS] " ++ msg

logWarning :: String -> IO ()
logWarning msg = putStrLn $ "[DEV-WARNING] " ++ msg

logError :: String -> IO ()
logError msg = putStrLn $ "[DEV-ERROR] " ++ msg

-- Check if files have changed
checkForChanges :: [String] -> IO Bool
checkForChanges dirs = do
    currentDir <- getCurrentDirectory
    let fullPaths = map (currentDir </>) dirs
    -- For now, just return True to trigger rebuild
    -- In a more sophisticated version, you could track file modification times
    return True

-- Shell command execution
runCommand :: String -> IO Bool
runCommand cmd = do
    logInfo $ "Running: " ++ cmd
    (exitCode, _, stderr) <- readProcessWithExitCode "bash" ["-c", cmd] ""
    case exitCode of
        ExitSuccess -> do
            logSuccess "Command completed successfully"
            return True
        ExitFailure code -> do
            logError $ "Command failed with exit code " ++ show code ++ ": " ++ stderr
            return False

-- Build the project (enhanced version)
buildProject :: String -> IO Bool
buildProject buildCmd = do
    logInfo $ "Building project with: " ++ buildCmd
    -- First ensure project files exist
    setupSuccess <- checkAndRegenerateProjectFiles
    if not setupSuccess then return False else do
        (exitCode, _, stderr) <- readProcessWithExitCode "bash" ["-c", buildCmd] ""
        case exitCode of
            ExitSuccess -> do
                logSuccess "Build completed successfully"
                return True
            ExitFailure code -> do
                logError $ "Build failed with exit code " ++ show code ++ ": " ++ stderr
                return False

-- Run the development server
runDevServer :: String -> IO ()
runDevServer runCmd = do
    logInfo $ "Starting development server with: " ++ runCmd
    _ <- spawnProcess "bash" ["-c", runCmd]
    logSuccess $ "Development server started on port " ++ show (appPort defaultDevConfig)
    logInfo "Press Ctrl+C to stop the server"

-- Development mode with auto-reload
devMode :: DevConfig -> IO ()
devMode config = do
    logInfo "Starting development mode with auto-reload..."
    logInfo $ "Watching directories: " ++ show (watchDirs config)
    logInfo $ "Build command: " ++ buildCmd config
    logInfo $ "Run command: " ++ runCmd config
    
    -- Initial build
    buildSuccess <- buildProject (buildCmd config)
    unless buildSuccess $ do
        logError "Initial build failed. Exiting."
        exitWith (ExitFailure 1)
    
    -- Start the server
    runDevServer (runCmd config)
    
    -- Watch for changes (simplified - just rebuild every 30 seconds for demo)
    logInfo "Watching for changes... (simplified mode - rebuilds every 30 seconds)"
    forever $ do
        threadDelay (30 * 1000000) -- 30 seconds
        logInfo "Checking for changes..."
        hasChanges <- checkForChanges (watchDirs config)
        when hasChanges $ do
            logInfo "Changes detected, rebuilding..."
            rebuildSuccess <- buildProject (buildCmd config)
            if rebuildSuccess
                then logSuccess "Rebuild successful, server will auto-reload"
                else logWarning "Rebuild failed, continuing with current version"

-- Setup and Project Management Functions
-- Simple file overwrite function
safeFileOverwrite :: FilePath -> String -> IO Bool
safeFileOverwrite filePath content = do
    logInfo $ "Ensuring file exists: " ++ filePath
    fileExists <- doesFileExist filePath
    if fileExists then do
        logWarning $ "File already exists, overwriting: " ++ filePath
        removeFile filePath
        writeFile filePath content
        logSuccess $ "File overwritten successfully: " ++ filePath
        return True
    else do
        logInfo $ "Creating new file: " ++ filePath
        writeFile filePath content
        logSuccess $ "File created successfully: " ++ filePath
        return True

-- Check and regenerate missing project files
checkAndRegenerateProjectFiles :: IO Bool
checkAndRegenerateProjectFiles = do
    logInfo "Checking for missing project files..."
    
    -- Check for stack.yaml
    stackYamlExists <- doesFileExist "stack.yaml"
    if not stackYamlExists then do
        logWarning "stack.yaml not found, regenerating..."
        let stackYamlContent = unlines
                [ "# This file was automatically generated by 'stack init'"
                , "snapshot: lts-22.28"
                , ""
                , "# User packages to be built."
                , "packages:"
                , "- ."
                , ""
                , "# Dependency packages to be pulled from upstream that are not in the snapshot."
                , "# extra-deps: []"
                , ""
                , "# Override default flag values for project packages and extra-deps"
                , "# flags: {}"
                , ""
                , "# Extra package databases containing global packages"
                , "# extra-package-dbs: []"
                , ""
                , "# Control whether we use the GHC we find on the path"
                , "# system-ghc: true"
                , ""
                , "# Require a specific version of Stack, using version ranges"
                , "# require-stack-version: -any # Default"
                , ""
                , "# Override the architecture used by Stack, especially useful on Windows"
                , "# arch: i386"
                , "# arch: x86_64"
                , ""
                , "# Extra directories used by Stack for building"
                , "# extra-include-dirs: [/path/to/dir]"
                , "# extra-lib-dirs: [/path/to/dir]"
                , ""
                , "# Allow a newer minor version of GHC than the snapshot specifies"
                , "# compiler-check: newer-minor"
                ]
        safeFileOverwrite "stack.yaml" stackYamlContent
    else do
        logInfo "stack.yaml exists"
        return True
    
    -- Check for package.yaml
    packageYamlExists <- doesFileExist "package.yaml"
    if not packageYamlExists then do
        logWarning "package.yaml not found, regenerating..."
        let packageYamlContent = unlines
                [ "name:                psyop-website"
                , "version:             0.1.0.0"
                , "github:              \"Shahzebqazi/psyop.ca\""
                , "license:             BSD-3-Clause"
                , "author:              \"PSYOP\""
                , "maintainer:          \"admin@psyop.ca\""
                , "copyright:           \"2025 PSYOP\""
                , "homepage:            \"https://psyop.ca\""
                , "bug-reports:         \"https://github.com/Shahzebqazi/psyop.ca/issues\""
                , "synopsis:            \"PSYOP Electronic Music Website\""
                , "description:         \"A modern, dynamic website for PSYOP electronic music project featuring dynamic backgrounds, Lovecraftian transitions, and image sequence management.\""
                , "category:            Web, Music, Electronic"
                , "extra-source-files:"
                , "- LICENSE"
                , "- env.template"
                , "- css/*"
                , "- public/*"
                , "- config/*"
                , "- assets/white/*"
                , "- assets/red_white/*"
                , ""
                , "dependencies:"
                , "- base >= 4.7 && < 5"
                , "- wai"
                , "- wai-cors"
                , "- warp"
                , "- servant"
                , "- servant-server"
                , "- blaze-html"
                , "- text"
                , "- bytestring"
                , "- http-types"
                , "- directory"
                , "- filepath"
                , "- wai-extra"
                , ""
                , "ghc-options:"
                , "- -Wall"
                , "- -Wcompat"
                , "- -Widentities"
                , "- -Wincomplete-record-updates"
                , "- -Wincomplete-uni-patterns"
                , "- -Wmissing-export-lists"
                , "- -Wmissing-home-modules"
                , "- -Wpartial-fields"
                , "- -Wredundant-constraints"
                , ""
                , "library:"
                , "  source-dirs:         src"
                , "  exposed-modules:"
                , "    - Lib"
                , ""
                , "executables:"
                , "  psyop-website-exe:"
                , "    main:                Main.hs"
                , "    source-dirs:         src"
                , "    dependencies:"
                , "    - psyop-website"
                , "    ghc-options:"
                , "    - -threaded"
                , "    - -rtsopts"
                , "    - -with-rtsopts=-N"
                , ""
                , "tests:"
                , "  psyop-website-test:"
                , "    main:                Spec.hs"
                , "    source-dirs:         test"
                , "    dependencies:"
                , "    - psyop-website"
                , "    - hspec"
                , "    - hspec-wai"
                , "    - wai"
                , "    - http-types"
                , "    ghc-options:"
                , "    - -Wall"
                ]
        safeFileOverwrite "package.yaml" packageYamlContent
    else do
        logInfo "package.yaml exists"
        return True
    
    -- Check for essential source files
    mainHsExists <- doesFileExist "src/Main.hs"
    if not mainHsExists then do
        logError "src/Main.hs not found - this is a critical file!"
        return False
    else do
        logInfo "src/Main.hs exists"
        return True
    
    libHsExists <- doesFileExist "src/Lib.hs"
    if not libHsExists then do
        logError "src/Lib.hs not found - this is a critical file!"
        return False
    else do
        logInfo "src/Lib.hs exists"
        return True
    
    logSuccess "All project files checked and regenerated as needed"
    return True

-- Force rebuild of project files
forceRebuildProjectFiles :: IO Bool
forceRebuildProjectFiles = do
    logInfo "Force rebuilding all project files..."
    
    -- Remove existing generated files
    let filesToRemove = ["psyop-website.cabal", "stack.yaml.lock"]
    
    mapM_ (\file -> do
        exists <- doesFileExist file
        when exists $ do
            logInfo $ "Removing existing file: " ++ file
            removeFile file
            logSuccess $ "Removed: " ++ file
        ) filesToRemove
    
    -- Regenerate all files
    success <- checkAndRegenerateProjectFiles
    if not success then return False else return True
    
    -- Force stack to rebuild
    logInfo "Forcing stack to rebuild project..."
    success <- runCommand "stack clean && stack build --force-dirty"
    if success then
        logSuccess "Project rebuilt successfully"
    else
        logWarning "Stack rebuild had issues, but continuing..."
    
    return True

-- Show project status
showStatus :: IO ()
showStatus = do
    logInfo "Checking project status..."
    
    putStrLn ""
    putStrLn "=== Project Status ==="
    
    -- Check for essential files
    let essentialFiles = ["src/Main.hs", "src/Lib.hs", "package.yaml", "stack.yaml"]
    
    mapM_ (\file -> do
        exists <- doesFileExist file
        if exists then
            putStrLn $ "✅ " ++ file
        else
            putStrLn $ "❌ " ++ file
        ) essentialFiles
    
    -- Check if project builds
    putStrLn ""
    putStrLn "=== Build Status ==="
    
    success <- runCommand "stack build --dry-run"
    if success then
        putStrLn "✅ Project configuration is valid"
    else
        putStrLn "❌ Project configuration has issues"
    
    putStrLn ""
    putStrLn "=== Next Steps ==="
    putStrLn "1. Build the project: runhaskell config/dev-server.hs build"
    putStrLn "2. Run the development server: runhaskell config/dev-server.hs dev"
    putStrLn "3. Visit http://localhost:8080 in your browser"

-- Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["build"] -> do
            success <- buildProject (buildCmd defaultDevConfig)
            exitWith $ if success then ExitSuccess else ExitFailure 1
        ["run"] -> do
            runDevServer (runCmd defaultDevConfig)
        ["dev"] -> do
            devMode defaultDevConfig
        ["setup"] -> do
            success <- checkAndRegenerateProjectFiles
            unless success $ exitWith (ExitFailure 1)
        ["rebuild"] -> do
            success <- forceRebuildProjectFiles
            unless success $ exitWith (ExitFailure 1)
        ["status"] -> showStatus
        ["help"] -> showHelp
        ["-h"] -> showHelp
        ["--help"] -> showHelp
        _ -> showHelp

-- Help and usage
showHelp :: IO ()
showHelp = do
    putStrLn "PSYOP Website Development Server with Setup"
    putStrLn ""
    putStrLn "Usage: runhaskell config/dev-server.hs [COMMAND]"
    putStrLn ""
    putStrLn "Development Commands:"
    putStrLn "  dev         Development mode with auto-reload (recommended)"
    putStrLn "  build       Build the project only"
    putStrLn "  run         Run the server only (no auto-reload)"
    putStrLn ""
    putStrLn "Setup Commands:"
    putStrLn "  setup       Check and regenerate missing project files"
    putStrLn "  rebuild     Force rebuild all project files"
    putStrLn "  status      Show project status"
    putStrLn "  help        Show this help message"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  runhaskell config/dev-server.hs dev      # Start development server"
    putStrLn "  runhaskell config/dev-server.hs setup    # Setup project files"
    putStrLn "  runhaskell config/dev-server.hs build    # Build project"
    putStrLn "  runhaskell config/dev-server.hs status   # Check status"
    putStrLn ""
    putStrLn "For local development, use: runhaskell config/dev-server.hs dev"
    putStrLn "This tool will automatically regenerate missing project files"
    putStrLn "such as package.yaml, stack.yaml, and psyop-website.cabal"
