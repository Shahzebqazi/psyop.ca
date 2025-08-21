{-# LANGUAGE OverloadedStrings #-}

module Models
    ( defaultConfig
    , getServerPort
    , generateOptimizedWallpaper
    , BackgroundType(..)
    , BackgroundSystem(..)
    , createBackgroundSystem
    , getBackgroundPriority
    , generateBackgroundFallback
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.IO (readFile)
import qualified Prelude as P
-- import Text.Blaze.Html.Renderer.String (renderHtml)
-- import qualified Components.WebGLBackground as WebGL

-- ============================================================================
-- SERVER CONFIGURATION
-- ============================================================================

-- Default server configuration
data ServerConfig = ServerConfig
    { serverPort :: P.Int
    , serverHost :: Text
    , environment :: Text
    } deriving (P.Show, P.Eq)

-- Default configuration
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
    { serverPort = 8080
    , serverHost = "localhost"
    , environment = "development"
    }

-- Get server port from config
getServerPort :: ServerConfig -> P.Int
getServerPort = serverPort

-- ============================================================================
-- LAYERED BACKGROUND SYSTEM
-- ============================================================================

-- Background types in priority order
data BackgroundType = 
    WebGLBackground    -- Primary: Hardware-accelerated, interactive
    | AsciiWallpaper   -- Fallback: ASCII art with content variety
    | GradientFallback -- Final: Simple CSS gradient
    deriving (P.Show, P.Eq, P.Ord)

-- Background system configuration
data BackgroundSystem = BackgroundSystem
    { primaryBackground :: BackgroundType
    , fallbackBackground :: BackgroundType
    , finalFailsafe :: BackgroundType
    , webglEnabled :: P.Bool
    , asciiEnabled :: P.Bool
    , gradientEnabled :: P.Bool
    } deriving (P.Show, P.Eq)

-- Create default background system
createBackgroundSystem :: BackgroundSystem
createBackgroundSystem = BackgroundSystem
    { primaryBackground = WebGLBackground
    , fallbackBackground = AsciiWallpaper
    , finalFailsafe = GradientFallback
    , webglEnabled = P.True
    , asciiEnabled = P.True
    , gradientEnabled = P.True
    }

-- Get background priority based on system state
getBackgroundPriority :: BackgroundSystem -> [BackgroundType]
getBackgroundPriority system = 
    let 
        -- Check what's available and enabled
        webgl = if webglEnabled system P.&& supportsWebGL then [WebGLBackground] else []
        ascii = if asciiEnabled system then [AsciiWallpaper] else []
        gradient = if gradientEnabled system then [GradientFallback] else []
        
        -- Combine in priority order
        priorities = webgl P.++ ascii P.++ gradient
    in priorities

-- Check if WebGL is supported (placeholder for now)
supportsWebGL :: P.Bool
supportsWebGL = P.False  -- Temporarily disabled to test ASCII wallpaper fixes

-- Generate appropriate background fallback
generateBackgroundFallback :: BackgroundType -> P.Int -> P.IO P.String
generateBackgroundFallback bgType seed = case bgType of
    WebGLBackground -> generateWebGLBackground seed
    AsciiWallpaper -> generateOptimizedWallpaper seed
    GradientFallback -> generateGradientBackground seed

-- Generate WebGL background (placeholder - will use component later)
generateWebGLBackground :: P.Int -> P.IO P.String
generateWebGLBackground seed = do
    P.return ("<div class=\"webgl-background\" data-seed=\"" P.++ P.show seed P.++ "\">" P.++
                "<canvas id=\"webgl-canvas\"></canvas>" P.++
                "<div class=\"webgl-loading\">Loading WebGL Background...</div>" P.++
                "</div>")

-- Generate gradient background (final failsafe)
generateGradientBackground :: P.Int -> P.IO P.String
generateGradientBackground seed = do
    P.return ("<div class=\"gradient-failsafe\" style=\"" P.++
                "position: absolute; top: 0; left: 0; width: 100%; height: 100%; " P.++
                "background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 50%, #1a1a1a 100%); " P.++
                "z-index: 1;\"></div>")

-- ============================================================================
-- SIMPLIFIED ASCII WALLPAPER SYSTEM
-- ============================================================================

-- Load psyop.txt content for ASCII wallpaper
loadPsyopContent :: P.IO [P.String]
loadPsyopContent = do
    content <- readFile "psyop.txt"
    P.return (P.lines content)

-- Generate optimized ASCII wallpaper content
generateOptimizedWallpaper :: P.Int -> P.IO P.String
generateOptimizedWallpaper seed = do
    content <- loadPsyopContent
    let 
        -- Process lines to extract only definitions (remove language names)
        processedContent = P.map extractDefinition content
        -- Take all available content for maximum coverage
        selectedContent = processedContent
        -- Add more assembly code and ACCESS_DENIED messages for better density
        assemblyCode = ["MOV AX, BX", "ADD CX, DX", "JMP 0x4000", "CALL 0x2000", "RET", "PUSH AX", "POP BX", "XOR AX, AX", "SUB DX, CX", "MUL BX", "DIV CX", "AND AX, BX", "OR CX, DX", "XOR SI, DI", "MOV SI, 0x1000", "MOV DI, 0x2000", "CMP AX, BX", "JE 0x3000", "JNE 0x4000", "LOOP 0x1000", "STOSB", "LODSB", "REP MOVSB", "REP STOSB", "CLC", "STC", "CLI", "STI", "CLD", "STD", "CMC", "LAHF", "SAHF", "PUSHF", "POPF", "CBW", "CWD", "AAA", "AAS", "DAA", "DAS", "AAM", "AAD", "XLAT", "IN AL, DX", "OUT DX, AL", "WAIT", "HLT", "LOCK", "ESC", "NOP", "STI", "CLI"]
        accessDenied = ["[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]", "[ACCESS_DENIED]"]
        -- Create a much larger content pool for dynamic scaling
        baseContent = interleaveContent selectedContent assemblyCode accessDenied
        -- Repeat the base content many times to ensure coverage on all screen sizes
        -- This creates a massive pool that will fill any viewport
        massiveContent = P.concat (P.replicate 50 baseContent)
        -- Create continuous text flow that will wrap naturally
        continuousText = P.unwords massiveContent
    P.return (renderContinuousWallpaper continuousText)

-- Extract definition part, removing language name prefix
extractDefinition :: P.String -> P.String
extractDefinition line = 
    case P.dropWhile (P./= ':') line of
        (':':' ':rest) -> rest
        (':':rest) -> rest
        _ -> line  -- If no colon found, return original

-- Interleave different content types
interleaveContent :: [P.String] -> [P.String] -> [P.String] -> [P.String]
interleaveContent defs assembly denied = 
    let         -- Create a much larger pattern to ensure coverage on all screen sizes
        basePattern = [0, 0, 0, 0, 1, 0, 0, 2, 0, 0, 1, 0, 0, 0, 1, 0, 2, 0, 0, 0]  -- More varied distribution
        -- Repeat the pattern many times to ensure coverage on large screens
        pattern = P.concat (P.replicate 200 basePattern)  -- 4000 items for massive coverage
        allContent = (defs, assembly, denied)
    in interleaveByPattern pattern allContent (0, 0, 0)

-- Helper for interleaving by pattern
interleaveByPattern :: [P.Int] -> ([P.String], [P.String], [P.String]) -> (P.Int, P.Int, P.Int) -> [P.String]
interleaveByPattern [] _ _ = []
interleaveByPattern (p:ps) (defs, assembly, denied) (di, ai, deni) =
    case p of
        0 -> if di P.< P.length defs 
             then (defs P.!! di) : interleaveByPattern ps (defs, assembly, denied) (di P.+ 1, ai, deni)
             else interleaveByPattern ps (defs, assembly, denied) (di, ai, deni)
        1 -> if ai P.< P.length assembly 
             then (assembly P.!! ai) : interleaveByPattern ps (defs, assembly, denied) (di, ai P.+ 1, deni)
             else interleaveByPattern ps (defs, assembly, denied) (di, ai, deni)
        2 -> if deni P.< P.length denied 
             then (denied P.!! deni) : interleaveByPattern ps (defs, assembly, denied) (di, ai, deni P.+ 1)
             else interleaveByPattern ps (defs, assembly, denied) (di, ai, deni)
        _ -> interleaveByPattern ps (defs, assembly, denied) (di, ai, deni)

-- Create font chunks with different classes
createFontChunks :: P.String -> [(P.String, P.String)]  -- (content, font_class)
createFontChunks text = 
    let words' = P.words text
        fonts = P.cycle ["font-courier", "font-consolas", "font-monaco"]
        chunkSize = 8  -- words per chunk
        chunks = chunksOf chunkSize words'
    in P.zip (P.map (P.unwords) chunks) fonts

-- Split list into chunks of specified size
chunksOf :: P.Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = P.take n xs : chunksOf n (P.drop n xs)

-- Render continuous wallpaper that fills the viewport
renderContinuousWallpaper :: P.String -> P.String
renderContinuousWallpaper text = 
    let words' = P.words text
        fonts = P.cycle ["font-courier", "font-consolas", "font-monaco"]
        formatWord (word, fontClass) = 
            if word P.== "[ACCESS_DENIED]"
            then "<span class=\"access-denied\">" P.++ word P.++ "</span>"
            else "<span class=\"" P.++ fontClass P.++ "\">" P.++ word P.++ "</span>"
        wordFontPairs = P.zip words' fonts
        formattedWords = P.map formatWord wordFontPairs
    in P.unwords formattedWords


