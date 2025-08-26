{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module App (app, FallbackEnv(..), loadFallbackEnv) where

import Network.Wai (Application, Response, Request, responseLBS, pathInfo, requestHeaders, isSecure, rawPathInfo, rawQueryString, queryString)
import Network.HTTP.Types (RequestHeaders)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types (status200, status404, status308)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.List (isSuffixOf, isPrefixOf)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import GHC.Generics (Generic)
import qualified Data.Yaml as Y
import Data.Aeson (FromJSON(..), withObject, (.:))

-- Helper function to parse query string
parseQueryString :: String -> [(String, String)]
parseQueryString query = 
    case break (== '=') query of
        (key, '=':value) -> [(key, value)]
        _ -> []

-- Fallback background functions moved to Models.hs for better organization

-- Simple environment for fallback content, cached at startup
data SocialLink = SocialLink
    { linkLabel :: T.Text
    , linkUrl   :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON SocialLink where
    parseJSON = withObject "SocialLink" $ \o -> do
        linkLabel <- o .: "label"
        linkUrl   <- o .: "url"
        pure SocialLink {.. }

data Site = Site
    { siteName                 :: T.Text
    , siteSubtitle             :: T.Text
    , siteHeroImage            :: T.Text
    , siteBio                  :: T.Text
    , siteSocialLinks          :: [SocialLink]
    , siteDefinitionsFile      :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON Site where
    parseJSON = withObject "Site" $ \o -> do
        siteName            <- o .: "name"
        siteSubtitle        <- o .: "subtitle"
        siteHeroImage       <- o .: "hero_image"
        siteBio             <- o .: "bio"
        siteSocialLinks     <- o .: "social_links"
        siteDefinitionsFile <- o .: "random_definitions_file"
        pure Site {..}

newtype SiteConfig = SiteConfig { site :: Site } deriving (Show, Eq)

instance FromJSON SiteConfig where
    parseJSON = withObject "SiteConfig" $ \o -> do
        s <- o .: "site"
        pure (SiteConfig s)

data FallbackEnv = FallbackEnv
    { fallbackDefinitions :: [T.Text]
    , rrIndexRef          :: IORef Int
    , siteConfig          :: Site
    }

-- Load definitions from psyop.txt once at startup
loadFallbackEnv :: IO FallbackEnv
loadFallbackEnv = do
    site <- loadSite
    defs <- loadDefinitions (T.unpack (siteDefinitionsFile site))
    idxRef <- newIORef 0
    pure FallbackEnv { fallbackDefinitions = defs, rrIndexRef = idxRef, siteConfig = site }

loadSite :: IO Site
loadSite = do
    let path = "private/content/site.yaml"
    exists <- doesFileExist path
    if not exists
        then pure defaultSite
        else do
            parsed <- Y.decodeFileEither path :: IO (Either Y.ParseException SiteConfig)
            case parsed of
                Right (SiteConfig s) -> pure s
                Left _err            -> pure defaultSite

loadDefinitions :: FilePath -> IO [T.Text]
loadDefinitions path = do
    fileExists <- doesFileExist path
    if fileExists
        then do
            content <- TIO.readFile path
            let rawLines = T.lines content
                cleaned  = Prelude.map extractDefinitionT rawLines
                nonEmpty = Prelude.filter (not . T.null . T.strip) cleaned
            pure (if null nonEmpty then [defaultDefinition] else nonEmpty)
        else pure [defaultDefinition]

defaultSite :: Site
defaultSite = Site
    { siteName = "Psyop"
    , siteSubtitle = "lite edition"
    , siteHeroImage = "assets/graphics/white/promo_1.jpg"
    , siteBio = T.unlines
        [ "Psyop (ˈsaɪ.ɑp) is a 4 piece Metal Machine from Toronto, Canada."
        , "Rooted in nu-metal foundations with addictive hooks and ruthless breakdowns."
        , "Psyop is James, Max, Miles and Willy."
        ]
    , siteSocialLinks =
        [ SocialLink { linkLabel = "Instagram", linkUrl = "https://instagram.com/psyopband" }
        , SocialLink { linkLabel = "TikTok",     linkUrl = "https://tiktok.com/@psyopsucks" }
        , SocialLink { linkLabel = "Spotify",    linkUrl = "https://open.spotify.com/artist/2bRWBW2Km3N9MSXY90QKb7?si=oVutT8ZFTxylihcAk-_sHg" }
        ]
    , siteDefinitionsFile = "psyop.txt"
    }

-- Get next definition via round-robin
nextDefinition :: FallbackEnv -> IO T.Text
nextDefinition env = do
    let defs = fallbackDefinitions env
        n    = length defs
    if n <= 1
        then pure (Prelude.head defs)
        else do
            i <- atomicModifyIORef' (rrIndexRef env) (\i -> let j = (i + 1) `mod` n in (j, j))
            pure (defs !! i)

-- Extract definition part after the first ':' if present
extractDefinitionT :: T.Text -> T.Text
extractDefinitionT line =
    case T.breakOn ":" line of
        (before, rest) -> if T.null rest
            then T.strip line
            else T.strip (T.drop 1 rest) -- drop the ':'

defaultDefinition :: T.Text
defaultDefinition = "Psychological Operation - Military operation designed to influence emotions, attitudes, and behavior of target audiences to support national objectives."

-- WAI application serving enhanced MenuBar with mobile support
app :: FallbackEnv -> Application
app env request respond = do
    -- Canonical redirects: http->https and non-www->www (controlled by env vars)
    redirectConfigured <- lookupEnv "REDIRECT_HTTPS"
    canonicalConfigured <- lookupEnv "WWW_CANONICAL"
    let httpsRedirectEnabled = maybe False truthy redirectConfigured
        wwwCanonicalEnabled  = maybe False truthy canonicalConfigured
        secure               = isSecure request
        hostHeader           = getHostHeader (requestHeaders request)
        pathBs               = rawPathInfo request
        queryBs              = rawQueryString request
        path                 = B8.unpack pathBs
        pathAndQuery         = path ++ B8.unpack queryBs
        needsWww h           = toLowerStr h /= "www.psyop.ca"
        isRedirectException  = path == "/health" || isAcmePath path
        maybeRedirect = if isRedirectException
            then Nothing
            else case (hostHeader, httpsRedirectEnabled, wwwCanonicalEnabled) of
                (Just h, _, True) | needsWww (B8.unpack h) -> Just (B8.pack ("https://www.psyop.ca" ++ pathAndQuery))
                (Just h, True, _) | not secure ->
                    let targetHost = if wwwCanonicalEnabled then "www.psyop.ca" else B8.unpack h
                    in Just (B8.pack ("https://" ++ targetHost ++ pathAndQuery))
                _ -> Nothing
    case maybeRedirect of
        Just loc -> respond $ responseLBS status308 [("Location", loc)] ""
        Nothing -> do
            let pathSegs = pathInfo request
            case pathSegs of
                -- Fallback-only site routes
                [] -> do
                    html <- renderFallbackPage env
                    respond $ waiResponse (htmlResponse html)
                ["index"] -> do
                    html <- renderFallbackPage env
                    respond $ waiResponse (htmlResponse html)
                ["index.html"] -> do
                    html <- renderFallbackPage env
                    respond $ waiResponse (htmlResponse html)
                ["lite"] -> do
                    html <- renderFallbackPage env
                    respond $ waiResponse (htmlResponse html)
                ["lite.html"] -> do
                    html <- renderFallbackPage env
                    respond $ waiResponse (htmlResponse html)
                ["home"] -> do
                    html <- renderFallbackPage env
                    respond $ waiResponse (htmlResponse html)
                
                -- Removed production-only endpoints (background generation, CSS, enhanced site)
                -- SEO files
                ["robots.txt"] -> do
                    response <- serveTextFile "public/robots.txt" "text/plain"
                    respond response
                ["sitemap.xml"] -> do
                    response <- serveTextFile "public/sitemap.xml" "application/xml"
                    respond response

                ["health"] -> do
                    respond $ responseLBS status200 [("Content-Type", "text/plain")] "ok"
                
                -- Static asset routes
                ["assets", "album-covers", filename] -> do
                    let filePath = "assets/album-covers/" ++ T.unpack filename
                    response <- serveStaticFile filePath
                    respond response
                
                ["assets", "graphics", "white", filename] -> do
                    let filePath = "assets/graphics/white/" ++ T.unpack filename
                    response <- serveStaticFile filePath
                    respond response
                
                ["assets", "graphics", "red_white", filename] -> do
                    let filePath = "assets/graphics/red_white/" ++ T.unpack filename
                    response <- serveStaticFile filePath
                    respond response
                
                ["assets", "graphics", filename] -> do
                    let filePath = "assets/graphics/" ++ T.unpack filename
                    response <- serveStaticFile filePath
                    respond response

                ["assets", "icons", "streaming", filename] -> do
                    let filePath = "assets/webdev/icons/streaming/" ++ T.unpack filename
                    response <- serveStaticFile filePath
                    respond response
                
                -- 404 for unmatched routes
                _ -> respond $ responseLBS status404 [("Content-Type", "text/html")] 
                    "<html><body><h1>404 - Page Not Found</h1><p><a href='/'>Return to Home</a></p></body></html>"

-- ACME challenge exceptions for HTTP->HTTPS redirect
isAcmePath :: String -> Bool
isAcmePath p = "/.well-known/acme-challenge/" `isPrefixOf` p

-- Render the minimal fallback page (black background, logo, lite edition, links, bio, randomized definition)
renderFallbackPage :: FallbackEnv -> IO Html
renderFallbackPage env = do
    defn <- nextDefinition env
    let s = siteConfig env
    pure $ H.docTypeHtml $ do
        H.head $ do
            H.meta ! A.charset "UTF-8"
            H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
            H.title $ toHtml (siteName s <> " - Lite")
            -- SEO Meta
            H.meta ! A.name "description" ! A.content (toValue (shortDescription (siteBio s)))
            H.link ! A.rel "canonical" ! A.href "https://www.psyop.ca/"
            H.meta ! H.customAttribute "property" "og:title" ! A.content (toValue (siteName s))
            H.meta ! H.customAttribute "property" "og:description" ! A.content (toValue (shortDescription (siteBio s)))
            H.meta ! H.customAttribute "property" "og:image" ! A.content (toValue (absoluteImageURL (siteHeroImage s)))
            -- Minimal inline CSS for black background and centered column
            H.style ! A.type_ "text/css" $ H.toHtml (T.unlines
                [ "html, body { margin:0; padding:0; background:#000; color:#f5f5dc; font-family: Arial, sans-serif; }"
                , ".container { min-height:100vh; display:flex; flex-direction:column; align-items:flex-start; justify-content:flex-start; gap:1rem; padding:2rem; text-align:left; }"
                , ".page-title { font-weight: 700; font-style: italic; font-size: 2rem; margin: 0; }"
                , ".title-row { display: flex; align-items: baseline; gap: 0.5rem; }"
                , ".subtitle { font-weight: 700; letter-spacing: 0.1em; text-transform: uppercase; }"
                , ".links { display:flex; flex-direction:row; gap:0.25rem; flex-wrap:wrap; align-items:center; }"
                , ".links a { color:#f5f5dc; text-decoration:none; border-bottom:1px solid rgba(245,245,220,0.3); padding-bottom:2px; }"
                , ".links a:hover { color:#ff6347; border-bottom-color:#ff6347; }"
                , ".bio { max-width: 600px; }"
                , ".definition { max-width: 600px; font-style: italic; color:#e6e6cc; }"
                , ".hero-image-sq { width: 300px; height: 300px; object-fit: cover; border: 2px solid #f5f5dc; box-shadow: 0 8px 32px rgba(245,245,220,0.2); background:#1a1a1a; }"
                ])
        H.body $ do
            H.div ! A.class_ "container" $ do
                H.div ! A.class_ "title-row" $ do
                    H.h1 ! A.class_ "page-title" $ toHtml (siteName s)
                    H.span ! A.class_ "subtitle" $ toHtml (siteSubtitle s)
                -- Inline menu items separated by ~ and left-aligned
                H.div ! A.class_ "links" $ do
                    renderLinks (siteSocialLinks s)
                H.div ! A.class_ "bio" $ do
                    mapM_ (\line -> H.p (toHtml line)) (T.lines (siteBio s))
                -- Hero image placed between bio and definition
                H.img ! A.class_ "hero-image-sq" ! A.src (toValue (siteHeroImage s)) ! A.alt "Psyop hero"
                H.div ! A.class_ "definition" $ H.toHtml defn

-- Render social links with ~ separators
renderLinks :: [SocialLink] -> Html
renderLinks [] = mempty
renderLinks (x:xs) = do
    renderLink x
    mapM_ (\l -> H.span " ~ " >> renderLink l) xs

renderLink :: SocialLink -> Html
renderLink (SocialLink lbl href) =
    H.a ! A.href (toValue href) ! A.target "_blank" ! A.rel "noopener noreferrer" $ toHtml lbl

-- Serve text-like files with a specified content type
serveTextFile :: FilePath -> BS.ByteString -> IO Response
serveTextFile filePath contentType = do
    exists <- doesFileExist filePath
    if exists
        then do
            fileContent <- BS.readFile filePath
            return $ responseLBS status200 [("Content-Type", contentType)] (LBS.fromStrict fileContent)
        else return $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

-- Fallback decision logic
shouldServeFallback :: Request -> IO Bool
shouldServeFallback req = do
    envSwitch <- lookupEnv "FALLBACK_MODE"
    let headerSwitch = lookup (CI.mk (B8.pack "X-Fallback-Mode")) (requestHeaders req)
        querySwitch  = queryParamTrue' "fallback" req
        bot          = isBotUserAgent req
        oldMobile    = isOldMobile req
        envTrue      = maybe False truthy envSwitch
        headerTrue   = maybe False (\v -> B8.map toLowerChar v == "1") headerSwitch
    pure (envTrue || headerTrue || querySwitch || bot || oldMobile)

truthy :: String -> Bool
truthy s = let ls = Prelude.map toLower s in ls == "1" || ls == "true" || ls == "yes"

toLowerStr :: String -> String
toLowerStr = Prelude.map toLower

toLowerChar :: Char -> Char
toLowerChar = toLower

queryParamTrue' :: BS.ByteString -> Request -> Bool
queryParamTrue' key req =
    let qs = Network.Wai.queryString req
    in any (\(k,v) -> k == key && maybe False (\x -> let t = B8.map toLowerChar x in t == "1" || t == "true") v) qs

isBotUserAgent :: Request -> Bool
isBotUserAgent req =
    let ua = fmap (B8.map toLowerChar) (lookup (CI.mk (B8.pack "User-Agent")) (requestHeaders req))
        bots = ["googlebot","bingbot","duckduckbot","yandexbot","baiduspider","ahrefsbot","semrushbot","petalbot"]
    in maybe False (\u -> any (`B8.isInfixOf` u) bots) ua

isOldMobile :: Request -> Bool
isOldMobile req =
    let ua = fmap (B8.map toLowerChar) (lookup (CI.mk (B8.pack "User-Agent")) (requestHeaders req))
        needles = ["nokia","blackberry","msie 6","msie 7","android 4."]
    in maybe False (\u -> any (`B8.isInfixOf` u) needles) ua

shortDescription :: T.Text -> T.Text
shortDescription t =
    let firstLine = take 160 (T.unpack (T.strip (headDef "" (T.lines t))))
    in T.pack firstLine

headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x:_) = x

absoluteImageURL :: T.Text -> T.Text
absoluteImageURL rel = if "/" `T.isPrefixOf` rel then T.concat ["https://www.psyop.ca", rel] else T.concat ["https://www.psyop.ca/", rel]

-- Extract Host or :authority header (HTTP/2)
getHostHeader :: RequestHeaders -> Maybe BS.ByteString
getHostHeader hdrs = case lookup (CI.mk (B8.pack "Host")) hdrs of
    Just h  -> Just h
    Nothing -> lookup (CI.mk (B8.pack ":authority")) hdrs

-- Render enhanced MenuBar with proper HTML structure
renderEnhancedMenuBar :: MenuBar -> Html
renderEnhancedMenuBar menuBar = 
    H.docTypeHtml $ do
        H.head $ do
            H.meta ! A.charset "UTF-8"
            H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
            H.title "PSYOP - Metal Band"
            H.link ! A.rel "stylesheet" ! A.href "/css/style.css"
            H.link ! A.rel "stylesheet" ! A.href "/css/components.css"
            H.link ! A.rel "stylesheet" ! A.href "/css/responsive.css"
        
        H.body $ do
            renderMenuBar menuBar
            
            -- Single page with multiple sections
            H.main ! A.class_ "main-content" $ do
                -- Home Section
                H.section ! A.id "home" ! A.class_ "page-section home-section" $ do
                    renderHomePage
                    -- ASCII wallpaper will be injected here via JavaScript
                    H.div ! A.id "ascii-wallpaper-inject" ! A.class_ "ascii-wallpaper-container" $ ""
                
                -- Links Section
                H.section ! A.id "links" ! A.class_ "page-section links-section" $ do
                    -- Social media content cards container
                    H.div ! A.class_ "links-container" $ do
                        H.div ! A.class_ "link-card title-style" $ do
                            H.a ! A.href "https://instagram.com/psyop" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.class_ "social-card" $ do
                                H.h3 "Links"
                        H.div ! A.class_ "link-card" $ do
                            H.a ! A.href "https://instagram.com/psyop" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.class_ "social-card" $ do
                                H.h3 "Instagram"
                                H.p "Follow us on Instagram"
                        H.div ! A.class_ "link-card" $ do
                            H.a ! A.href "https://tiktok.com/@psyop" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.class_ "social-card" $ do
                                H.h3 "TikTok"
                                H.p "Check out our TikTok"
                        H.div ! A.class_ "link-card" $ do
                            H.a ! A.href "https://open.spotify.com/artist/psyop" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.class_ "social-card" $ do
                                H.h3 "Spotify"
                                H.p "Listen on Spotify"
                        H.div ! A.class_ "link-card" $ do
                            H.a ! A.href "https://youtube.com/@psyop" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.class_ "social-card" $ do
                                H.h3 "YouTube"
                                H.p "Watch on YouTube"
                        H.div ! A.class_ "link-card" $ do
                            H.a ! A.href "https://music.apple.com/artist/psyop" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.class_ "social-card" $ do
                                H.h3 "Apple Music"
                                H.p "Stream on Apple Music"
                
                -- Shop Section (Commented out for future implementation)
                -- H.section ! A.id "shop" ! A.class_ "page-section shop-section" $ do
                --     H.div ! A.class_ "section-content" $ do
                --         H.h2 "Shop"
                --         H.p "Merchandise and music"
            
            -- Footer at the end of the page
            renderFooter mkFooter
            
            -- JavaScript for intelligent background system
            H.script ! A.type_ "text/javascript" $ 
                H.preEscapedToHtml $ unlines [
                    "// Intelligent Background System"
                    , "let currentBackground = null;"
                    , "let backgroundSeed = Date.now();"
                    , "let backgroundPriority = ['webgl', 'ascii', 'gradient'];"
                    , ""
                    , "// Check WebGL support"
                    , "function supportsWebGL() {"
                    , "    try {"
                    , "        const canvas = document.createElement('canvas');"
                    , "        return !!(window.WebGLRenderingContext && (canvas.getContext('webgl') || canvas.getContext('experimental-webgl')));"
                    , "    } catch (e) {"
                    , "        return false;"
                    , "    }"
                    , "}"
                    , ""
                    , "// Generate and inject intelligent background"
                    , "async function generateIntelligentBackground() {"
                    , "    try {"
                    , "        console.log('🎨 Generating intelligent background...');"
                    , "        const response = await fetch('/generate-background?seed=' + backgroundSeed);"
                    , "        if (response.ok) {"
                    , "            const background = await response.text();"
                    , "            const container = document.getElementById('ascii-wallpaper-inject');"
                    , "            if (container) {"
                    , "                container.innerHTML = background;"
                    , "                currentBackground = background;"
                    , "                console.log('✅ Background generated successfully');"
                    , "                "
                    , "                // Initialize WebGL if it's a WebGL background"
                    , "                if (background.includes('webgl-background')) {"
                    , "                    initializeWebGLBackground();"
                    , "                }"
                    , "            }"
                    , "        }"
                    , "    } catch (error) {"
                    , "        console.log('❌ Background generation failed, using ultimate fallback');"
                    , "        // Ultimate fallback to black background"
                    , "        const container = document.getElementById('ascii-wallpaper-inject');"
                    , "        if (container) {"
                    , "            container.innerHTML = '<div class=\"ultimate-fallback\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: #000000; z-index: 1;\"></div>';"
                    , "        }"
                    , "    }"
                    , "}"
                    , ""
                    , "// Initialize WebGL background (placeholder for future implementation)"
                    , "function initializeWebGLBackground() {"
                    , "    console.log('🚀 Initializing WebGL background...');"
                    , "    // TODO: Implement WebGL shaders and rendering"
                    , "    // For now, just show the loading message"
                    , "}"
                    , ""
                    ,                     "// Initialize background on page load"
                    , "document.addEventListener('DOMContentLoaded', () => {"
                    , "    generateIntelligentBackground();"
                    , "    initializeNavigationArrow();"
                    , "});"
                    , ""
                    , "// Navigation Arrow System"
                    , "let navigationArrow = null;"
                    , ""
                    , "function initializeNavigationArrow() {"
                    , "    // Create navigation arrow"
                    , "    navigationArrow = document.createElement('div');"
                    , "    navigationArrow.className = 'navigation-arrow';"
                    , "    navigationArrow.innerHTML = '<svg width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"M7 10l5 5 5-5\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/></svg>';"
                    , "    navigationArrow.addEventListener('click', () => {"
                    , "        scrollToSection('links');"
                    , "        // Hide arrow immediately when clicked"
                    , "        navigationArrow.style.opacity = '0';"
                    , "        navigationArrow.style.pointerEvents = 'none';"
                    , "    });"
                    , "    document.body.appendChild(navigationArrow);"
                    , "    "
                    , "    // Show arrow initially"
                    , "    navigationArrow.style.opacity = '1';"
                    , "}"
                    , ""
                    , "function updateNavigationArrow() {"
                    , "    if (!navigationArrow) return;"
                    , "    "
                    , "    const scrollPosition = window.scrollY;"
                    , "    const homeSection = document.getElementById('home');"
                    , "    "
                    , "    if (homeSection) {"
                    , "        const homeSectionBottom = homeSection.offsetTop + homeSection.offsetHeight;"
                    , "        const scrollThreshold = homeSectionBottom * 0.7; // Hide when 70% through home section"
                    , "        "
                    , "        // Hide arrow if scrolled past 70% of home section"
                    , "        if (scrollPosition > scrollThreshold) {"
                    , "            navigationArrow.style.opacity = '0';"
                    , "            navigationArrow.style.pointerEvents = 'none';"
                    , "        } else {"
                    , "            navigationArrow.style.opacity = '1';"
                    , "            navigationArrow.style.pointerEvents = 'auto';"
                    , "        }"
                    , "    }"
                    , "}"
                    , ""
                    , "// Smooth scrolling between sections"
                    , "function scrollToSection(sectionId) {"
                    , "    const section = document.getElementById(sectionId);"
                    , "    if (section) {"
                    , "        section.scrollIntoView({ behavior: 'smooth' });"
                    , "        updateActiveSection(sectionId);"
                    , "    }"
                    , "}"
                    , ""
                    , "// Update active section in navigation"
                    , "function updateActiveSection(activeId) {"
                    , "    const navItems = document.querySelectorAll('.nav-item');"
                    , "    navItems.forEach(item => {"
                    , "        item.classList.remove('active');"
                    , "        const link = item.querySelector('a');"
                    , "        if (link && link.getAttribute('href') === '#' + activeId) {"
                    , "            item.classList.add('active');"
                    , "        }"
                    , "    });"
                    , "}"
                    , ""
                    , "// Handle scroll events to update active section and navigation arrow"
                    , "window.addEventListener('scroll', () => {"
                    , "    const sections = ['home', 'links']; // shop commented out for future implementation"
                    , "    const scrollPosition = window.scrollY + window.innerHeight / 2;"
                    , ""
                    , "    sections.forEach(sectionId => {"
                    , "        const section = document.getElementById(sectionId);"
                    , "        if (section) {"
                    , "            const sectionTop = section.offsetTop;"
                    , "            const sectionBottom = sectionTop + section.offsetHeight;"
                    , "            if (scrollPosition >= sectionTop && scrollPosition < sectionBottom) {"
                    , "                updateActiveSection(sectionId);"
                    , "            }"
                    , "        }"
                    , "    });"
                    , "    "
                    , "    // Update navigation arrow visibility"
                    , "    updateNavigationArrow();"
                    , "});"
                    , ""
                    , "// Mobile menu toggle"
                    , "function toggleMobileMenu() {"
                    , "    const navLinks = document.querySelector('.nav-links');"
                    , "    const mobileToggle = document.querySelector('.mobile-menu-toggle');"
                    , "    if (navLinks && mobileToggle) {"
                    , "        navLinks.classList.toggle('active');"
                    , "        mobileToggle.classList.toggle('active');"
                    , "    }"
                    , "}"
                    , ""
                    , "// Close mobile menu when clicking outside"
                    , "document.addEventListener('click', (event) => {"
                    , "    const navLinks = document.querySelector('.nav-links');"
                    , "    const mobileToggle = document.querySelector('.mobile-menu-toggle');"
                    , "    if (navLinks && !navLinks.contains(event.target) && !mobileToggle.contains(event.target)) {"
                    , "        navLinks.classList.remove('active');"
                    , "        mobileToggle.classList.remove('active');"
                    , "    }"
                    , "});"
                    , ""
                    , "    // Psyop definition cycling functionality - Constrained to 6 lines max"
                    , "    const psyopDefinitions = ["
                    , "        'Psychological Operation - Military operation designed to influence emotions, attitudes, and behavior of target audiences to support national objectives.',"
                    , "        'Operación Psicológica - Operación militar diseñada para influir en emociones, actitudes y comportamiento de audiencias objetivo para apoyar objetivos nacionales.',"
                    , "        'Opération Psychologique - Opération militaire conçue pour influencer les émotions, les attitudes et le comportement des publics cibles pour soutenir les objectifs nationaux.',"
                    , "        'Operazione Psicologica - Un\\'operazione militare progettata per influenzare le emozioni, gli atteggiamenti e il comportamento del pubblico target per sostenere gli obiettivi nazionali.',"
                    , "        'Psicologische Operation - Eine militärische Operation, die darauf abzielt, die Emotionen, Einstellungen und das Verhalten von Zielgruppen zu beeinflussen, um nationale Ziele zu unterstützen.',"
                    , "        'Operação Psicológica - Uma operação militar projetada para influenciar as emoções, atitudes e comportamento de audiências-alvo para apoiar objetivos nacionais.',"
                    , "        'Psikolojik Operasyon - Ulusal hedefleri desteklemek için hedef kitlelerin duygularını, tutumlarını ve davranışlarını etkilemek için tasarlanmış askeri bir operasyon.',"
                    , "        'Операция Психологического Воздействия - Военная операция, предназначенная для влияния на эмоции, отношения и поведение целевых аудиторий для поддержки национальных целей.'"
                    , "    ];"
                    , ""
                    , "    let currentDefinitionIndex = 0;"
                    , "    const definitionElement = document.getElementById('psyop-definition');"
                    , ""
                    , "    function cyclePsyopDefinition() {"
                    , "        if (definitionElement) {"
                    , "            // Add glitch effect"
                    , "            definitionElement.classList.add('glitch');"
                    , ""
                    , "            // Change text after a short delay for glitch effect"
                    , "            setTimeout(() => {"
                    , "                currentDefinitionIndex = (currentDefinitionIndex + 1) % psyopDefinitions.length;"
                    , "                definitionElement.textContent = psyopDefinitions[currentDefinitionIndex];"
                    , ""
                    , "                // Remove glitch class after animation completes"
                    , "                setTimeout(() => {"
                    , "                    definitionElement.classList.remove('glitch');"
                    , "                }, 300);"
                    , "            }, 150);"
                    , "        }"
                    , "    }"
                    , ""
                    , "    // Start cycling every 4 seconds"
                    , "    if (definitionElement) {"
                    , "        setInterval(cyclePsyopDefinition, 4000);"
                    , "    }"
                ]
            


-- Helper function to create HTML response
htmlResponse :: Html -> LBS.ByteString
htmlResponse html = LBS.fromStrict $ encodeUtf8 $ T.pack $ renderHtml html

-- Helper function to create WAI response
waiResponse :: LBS.ByteString -> Response
waiResponse content = responseLBS status200 [("Content-Type", "text/html")] content

-- Helper function to create CSS response
cssResponse :: LBS.ByteString -> Response
cssResponse content = responseLBS status200 [("Content-Type", "text/css")] content

-- Helper function to serve static files
serveStaticFile :: String -> IO Response
serveStaticFile filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            fileContent <- BS.readFile filePath
            let contentType = case filePath of
                    path | ".jpg" `isSuffixOf` path || ".jpeg" `isSuffixOf` path -> "image/jpeg"
                    path | ".png" `isSuffixOf` path -> "image/png"
                    path | ".gif" `isSuffixOf` path -> "image/gif"
                    _ -> "application/octet-stream"
            return $ responseLBS status200 [("Content-Type", contentType)] $ LBS.fromStrict fileContent
        else
            return $ responseLBS status404 [("Content-Type", "text/plain")] $ LBS.fromStrict $ encodeUtf8 $ T.pack $ "File not found: " ++ filePath

-- Serve main CSS with enhanced styling
serveMainCSS :: LBS.ByteString
serveMainCSS = LBS.fromStrict $ encodeUtf8 $ T.pack $ unlines [
    "/* Main CSS for PSYOP Website */"
    , "body {"
    , "    margin: 0;"
    , "    padding: 0;"
    , "    font-family: 'Arial', sans-serif;"
    , "    background: #000;"
    , "    color: #f5f5dc;"
    , "    overflow-x: hidden;"
    , "    overscroll-behavior: none;"
    , "    display: flex;"
    , "    flex-direction: column;"
    , "    min-height: 100vh;"
    , "}"
    , ""
    , "html {"
    , "    overscroll-behavior: none;"
    , "}"
    , ""
    , "/* Main content layout */"
    , ".main-content {"
    , "    flex: 1;"
    , "    padding-top: 80px;"
    , "}"
    , ""
    , "/* Section styles */"
    , ".page-section {"
    , "    min-height: 100vh;"
    , "    position: relative;"
    , "    display: flex;"
    , "    align-items: center;"
    , "    justify-content: center;"
    , "}"
    , ""
    , "/* Fallback Gradient Background */"
    , ".fallback-background {"
    , "    position: absolute;"
    , "    top: 0;"
    , "    left: 0;"
    , "    width: 100%;"
    , "    height: 100%;"
    , "    z-index: -1;"
    , "    background: linear-gradient(180deg, #000000 0%, #333333 50%, #666666 100%);"
    , "}"
    , ""
    , "/* Animation System */"
    , ".animated-element {"
    , "    opacity: 0;"
    , "    transform: translateY(20px);"
    , "    transition: all 0.6s ease-out;"
    , "}"
    , ""
    , ".animated-element.fade-in {"
    , "    animation: fadeIn 0.6s ease-out forwards;"
    , "}"
    , ""
    , ".animated-element.slide-up {"
    , "    animation: slideUp 0.8s ease-out 0.2s forwards;"
    , "}"
    , ""
    , ".animated-element.pulse {"
    , "    animation: pulse 2s ease-in-out infinite;"
    , "}"
    , ""
    , "@keyframes fadeIn {"
    , "    from {"
    , "        opacity: 0;"
    , "        transform: translateY(20px);"
    , "    }"
    , "    to {"
    , "        opacity: 1;"
    , "        transform: translateY(0);"
    , "    }"
    , "}"
    , ""
    , "@keyframes slideUp {"
    , "    from {"
    , "        opacity: 0;"
    , "        transform: translateY(30px);"
    , "    }"
    , "    to {"
    , "        opacity: 1;"
    , "        transform: translateY(0);"
    , "    }"
    , "}"
    , ""
    , "/* Section Content */"
    , ".section-content {"
    , "    position: relative;"
    , "    z-index: 2;"
    , "    text-align: center;"
    , "    padding: 2rem;"
    , "    color: #f5f5dc;"
    , "}"
    , ""
    , "/* Section-specific styles - ASCII wallpaper will override these */"
    , ".home-section { background: #000000; position: relative; overflow: hidden; }"
    , ".links-section { background: #000000; position: relative; overflow: hidden; }"
    -- , ".shop-section { background: #000000; position: relative; overflow: hidden; }" -- Commented out for future implementation
    , ""
    , "/* Intelligent Background System Styles */"
    , ".webgl-background {"
    , "    position: absolute;"
    , "    top: 0;"
    , "    left: 0;"
    , "    width: 100%;"
    , "    height: 100%;"
    , "    z-index: 1;"
    , "    overflow: hidden;"
    , "    pointer-events: none;"
    , "    background: #000000;"
    , "}"
    , ""
    , ".webgl-canvas {"
    , "    width: 100%;"
    , "    height: 100%;"
    , "    display: block;"
    , "}"
    , ""
    , ".webgl-loading {"
    , "    position: absolute;"
    , "    top: 50%;"
    , "    left: 50%;"
    , "    transform: translate(-50%, -50%);"
    , "    color: #cccccc;"
    , "    font-family: 'Courier New', monospace;"
    , "    font-size: 14px;"
    , "    z-index: 2;"
    , "}"
    , ""
    , ".gradient-failsafe {"
    , "    position: absolute;"
    , "    top: 0;"
    , "    left: 0;"
    , "    width: 100%;"
    , "    height: 100%;"
    , "    z-index: 1;"
    , "    background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 50%, #1a1a1a 100%);"
    , "}"
    , ""
    , ".ultimate-fallback {"
    , "    position: absolute;"
    , "    top: 0;"
    , "    left: 0;"
    , "    width: 100%;"
    , "    height: 100%;"
    , "    z-index: 1;"
    , "    background: #000000;"
    , "}"
    , ""
    , "/* ASCII Wallpaper Styles (Fallback) */"
    , ".ascii-wallpaper {"
    , "    position: absolute;"
    , "    top: 0;"
    , "    left: 0;"
    , "    width: 100%;"
    , "    height: 100%;"
    , "    z-index: 1;"
    , "    overflow: hidden;"
    , "    pointer-events: none;"
    , "    background: #000000;"
    , "}"
    , ""
    , ".ascii-wallpaper-container {"
    , "    position: fixed;"
    , "    top: 100px;"
    , "    left: 0;"
    , "    width: 100%;"
    , "    height: calc(100vh - 120px);"
    , "    z-index: 1;"
    , "    overflow: hidden;"
    , "    pointer-events: none;"
    , "    background: #000000;"
    , "    padding: 10px 20px;"
    , "    box-sizing: border-box;"
    , "    line-height: 1.2;"
    , "    word-spacing: 1px;"
    , "    min-height: calc(100vh - 120px);"
    , "    white-space: normal;"
    , "    word-wrap: break-word;"
    , "    font-size: 10px;"
    , "}"
    , ""
    , ".ascii-content {"
    , "    color: #666666;"
    , "    font-size: 10px;"
    , "    line-height: 1.0;"
    , "    white-space: normal;"
    , "    margin: 0;"
    , "    padding: 8px;"
    , "    opacity: 0.9;"
    , "    user-select: none;"
    , "    word-wrap: break-word;"
    , "}"
    , ""
    , "/* Font variety for ASCII wallpaper */"
    , ".font-courier {"
    , "    font-family: 'Courier New', monospace;"
    , "    color: #666666;"
    , "    font-size: 9px;"
    , "    display: inline;"
    , "    margin-right: 1px;"
    , "    white-space: nowrap;"
    , "}"
    , ""
    , ".font-consolas {"
    , "    font-family: 'Consolas', 'Monaco', monospace;"
    , "    color: #666666;"
    , "    font-size: 9px;"
    , "    display: inline;"
    , "    margin-right: 1px;"
    , "    white-space: nowrap;"
    , "}"
    , ""
    , ".font-monaco {"
    , "    font-family: 'Monaco', 'Consolas', monospace;"
    , "    color: #666666;"
    , "    font-size: 9px;"
    , "    display: inline;"
    , "    margin-right: 1px;"
    , "    white-space: nowrap;"
    , "}"
    , ""
    , "/* ACCESS_DENIED styling - CSS handles all red text styling */"
    , ".access-denied {"
    , "    color: #ff6347 !important;"
    , "    font-weight: bold;"
    , "    text-shadow: 0 0 8px rgba(255, 99, 71, 0.6);"
    , "    background-color: rgba(255, 99, 71, 0.15);"
    , "    padding: 1px 3px;"
    , "    border-radius: 3px;"
    , "    border: 1px solid rgba(255, 99, 71, 0.3);"
    , "    display: inline-block;"
    , "    margin: 0 1px;"
    , "    animation: pulse-red 2s infinite;"
    , "}"
    , ""
    , "@keyframes pulse-red {"
    , "    0%, 100% { opacity: 1; }"
    , "    50% { opacity: 0.8; }"
    , "}"
    , ""
    , "/* Home Container Styles */"
    , ".home-container {"
    , "    position: relative;"
    , "    min-height: 100vh;"
    , "    display: flex;"
    , "    flex-direction: column;"
    , "    justify-content: center;"
    , "    align-items: center;"
    , "    text-align: center;"
    , "    color: #f5f5dc;"
    , "}"
    , ""
    , "/* Navigation Arrow Styles */"
    , ".navigation-arrow {"
    , "    position: fixed;"
    , "    bottom: 40px;"
    , "    left: 50%;"
    , "    transform: translateX(-50%);"
    , "    width: 48px;"
    , "    height: 48px;"
    , "    background: rgba(255, 255, 255, 0.1);"
    , "    color: #ffffff;"
    , "    border: 2px solid rgba(255, 255, 255, 0.3);"
    , "    border-radius: 8px;"
    , "    display: flex;"
    , "    align-items: center;"
    , "    justify-content: center;"
    , "    cursor: pointer;"
    , "    z-index: 1000;"
    , "    transition: all 0.3s ease;"
    , "    animation: float 3s ease-in-out infinite;"
    , "    backdrop-filter: blur(10px);"
    , "}"
    , ""
    , "/* Album Artwork Styles */"
    , ".album-artwork {"
    , "    margin-bottom: 2rem;"
    , "    text-align: center;"
    , "}"
    , ""
    , ".album-cover {"
    , "    max-width: 400px;"
    , "    width: 100%;"
    , "    height: auto;"
    , "    border: 2px solid #f5f5dc;"
    , "    box-shadow: 0 8px 32px rgba(245, 245, 220, 0.3);"
    , "    transition: all 0.3s ease;"
    , "    filter: grayscale(20%) contrast(110%);"
    , "    display: block;"
    , "    background: #1a1a1a;"
    , "    object-fit: cover;"
    , "    aspect-ratio: 1/1;"
    , "}"
    , ""
    , ".album-cover:hover {"
    , "    transform: scale(1.05);"
    , "    box-shadow: 0 12px 40px rgba(245, 245, 220, 0.5);"
    , "    filter: grayscale(0%) contrast(120%);"
    , "}"
    , ""
    , "/* Band Description Styling */"
    , ".black-title {"
    , "    background: #000000;"
    , "    color: #ffffff;"
    , "    text-align: left;"
    , "    padding: 1.5rem 2.5rem;"
    , "    margin: 3rem auto 2rem auto;"
    , "    max-width: 500px;"
    , "    width: 90%;"
    , "    font-size: 1rem;"
    , "    font-weight: 400;"
    , "    text-transform: none;"
    , "    letter-spacing: 0.02em;"
    , "    border-radius: 0;"
    , "    border-right: 2px solid #ffffff;"
    , "    border-bottom: 2px solid #ffffff;"
    , "    border-top: none;"
    , "    border-left: none;"
    , "    z-index: 10;"
    , "    box-sizing: border-box;"
    , "    line-height: 1.4;"
    , "    height: 26rem;"
    , "    overflow: hidden;"
    , "}"
    , ""

    , ""
    , "/* Links Title Styling */"
    , ".links-title {"
    , "    background: #ffff00;"
    , "    color: #000000;"
    , "    text-align: left;"
    , "    padding: 1rem 2.5rem;"
    , "    margin: 0 auto 2rem auto;"
    , "    max-width: 500px;"
    , "    width: 90%;"
    , "    font-size: 2.5rem;"
    , "    font-weight: 900;"
    , "    text-transform: uppercase;"
    , "    letter-spacing: 0.05em;"
    , "    border-radius: 0;"
    , "    border-right: 2px solid #ffffff;"
    , "    border-bottom: 2px solid #ffffff;"
    , "    outline: 3px solid #000000;"
    , "    z-index: 15;"
    , "    box-sizing: border-box;"
    , "    position: relative;"
    , "    display: block;"
    , "    clear: both;"
    , "    float: none;"
    , "}"
    , ""
    , "/* Social Media Cards Styling */"
    , ".links-container {"
    , "    display: flex;"
    , "    flex-direction: column;"
    , "    align-items: center;"
    , "    gap: 1rem;"
    , "    position: relative;"
    , "    z-index: 10;"
    , "    width: 100%;"
    , "    max-width: 500px;"
    , "    margin: 0 auto;"
    , "}"
    , ""
    , ".link-card {"
    , "    background: rgba(0, 0, 0, 0.6);"
    , "    padding: 1rem 2.5rem;"
    , "    margin: 0 auto 1rem auto;"
    , "    max-width: 500px;"
    , "    width: 90%;"
    , "    border-radius: 0;"
    , "    border-right: 2px solid #ffffff;"
    , "    border-bottom: 2px solid #ffffff;"
    , "    z-index: 15;"
    , "    box-sizing: border-box;"
    , "    text-align: center;"
    , "    position: relative;"
    , "}"
    , ""
    , ".link-card.title-style {"
    , "    background: #ffff00;"
    , "    color: #000000;"
    , "    text-align: left;"
    , "    padding: 1rem 2.5rem;"
    , "    margin: 0 auto 2rem auto;"
    , "    max-width: 500px;"
    , "    width: 90%;"
    , "    font-size: 2.5rem;"
    , "    font-weight: 900;"
    , "    text-transform: uppercase;"
    , "    letter-spacing: 0.05em;"
    , "    border-radius: 0;"
    , "    border-right: 2px solid #ffffff;"
    , "    border-bottom: 2px solid #ffffff;"
    , "    outline: 3px solid #000000;"
    , "    z-index: 15;"
    , "    box-sizing: border-box;"
    , "    position: relative;"
    , "    height: 4rem;"
    , "    display: flex;"
    , "    align-items: center;"
    , "}"
    , ""
    , ".social-card {"
    , "    text-decoration: none;"
    , "    color: #f5f5dc;"
    , "    display: block;"
    , "    transition: all 0.3s ease;"
    , "}"
    , ""
    , ".title-style .social-card {"
    , "    color: #000000;"
    , "}"
    , ""
    , ".title-style .social-card h3 {"
    , "    color: #000000;"
    , "    font-size: 2.5rem;"
    , "    font-weight: 900;"
    , "    text-transform: uppercase;"
    , "    letter-spacing: 0.05em;"
    , "    margin-bottom: 0.5em;"
    , "}"
    , ""
    , ".title-style .social-card p {"
    , "    color: #000000;"
    , "    font-size: 1rem;"
    , "    margin: 0;"
    , "}"
    , ""
    , ".social-card:hover {"
    , "    color: #ff6347;"
    , "    transform: translateY(-2px);"
    , "}"
    , ""
    , ".social-card h3 {"
    , "    font-size: 1.8rem;"
    , "    font-weight: bold;"
    , "    margin-bottom: 0.5em;"
    , "    text-transform: uppercase;"
    , "    letter-spacing: 0.1em;"
    , "}"
    , ""
    , ".social-card p {"
    , "    font-size: 1rem;"
    , "    color: #e6e6cc;"
    , "    margin: 0;"
    , "}"
    , ""
    , ".psyop-definition {"
    , "    color: #f5f5dc;"
    , "    font-size: 1rem;"
    , "    line-height: 1.4;"
    , "    display: block;"
    , "    margin-top: 0;"
    , "    transition: all 0.1s ease;"
    , "    width: 100%;"
    , "    max-width: 100%;"
    , "    word-wrap: break-word;"
    , "    overflow-wrap: break-word;"
    , "    hyphens: auto;"
    , "    max-width: 400px;"
    , "}"
    , ""
    , ".bio-line {"
    , "    min-height: 2rem;"
    , "    margin-bottom: 0.5rem;"
    , "    display: block;"
    , "    line-height: 1.4;"
    , "    overflow-wrap: break-word;"
    , "    word-wrap: break-word;"
    , "    hyphens: auto;"
    , "}"
    , ""
    , ".bio-line-1 {"
    , "    font-size: 1.1rem;"
    , "    font-weight: 500;"
    , "    margin-bottom: 0.8rem;"
    , "    line-height: 1.4;"
    , "}"
    , ""
    , ".bio-line-2, .bio-line-3 {"
    , "    font-size: 1rem;"
    , "    font-weight: 400;"
    , "}"
    , ""
    , ".bio-line-4 {"
    , "    font-size: 1rem;"
    , "    font-weight: 500;"
    , "    margin-top: 1rem;"
    , "}"
    , ""
    , ".bio-line-5 {"
    , "    font-size: 1rem;"
    , "    font-weight: 400;"
    , "    min-height: 2rem;"
    , "}"
    , ""
    , ".bio-line-6, .bio-line-7 {"
    , "    font-size: 1rem;"
    , "    font-weight: 400;"
    , "    min-height: 2rem;"
    , "}"
    , ""
    , ".bio-line-8 {"
    , "    font-size: 1rem;"
    , "    font-weight: 500;"
    , "    margin-top: 1rem;"
    , "    min-height: 2rem;"
    , "}"
    , ""

    , ""
    , ".psyop-definition.glitch {"
    , "    animation: glitchEffect 0.3s ease-in-out;"
    , "}"
    , ""
    , "@keyframes glitchEffect {"
    , "    0% {"
    , "        transform: translate(0);"
    , "        filter: hue-rotate(0deg);"
    , "    }"
    , "    20% {"
    , "        transform: translate(-2px, 2px);"
    , "        filter: hue-rotate(90deg);"
    , "        color: #ff0000;"
    , "    }"
    , "    40% {"
    , "        transform: translate(2px, -2px);"
    , "        filter: hue-rotate(180deg);"
    , "        color: #00ff00;"
    , "    }"
    , "    60% {"
    , "        transform: translate(-1px, 1px);"
    , "        filter: hue-rotate(270deg);"
    , "        color: #0000ff;"
    , "    }"
    , "    80% {"
    , "        transform: translate(1px, -1px);"
    , "        filter: hue-rotate(360deg);"
    , "        color: #ffff00;"
    , "    }"
    , "    100% {"
    , "        transform: translate(0);"
    , "        filter: hue-rotate(0deg);"
    , "        color: #f5f5dc;"
    , "    }"
    , "}"
    , ""
    , "/* Music Title Styling */"
    , ".music-title {"
    , "    background: #ffff00;"
    , "    color: #000000;"
    , "    text-align: left;"
    , "    padding: 1rem 2.5rem;"
    , "    margin: 0 auto 2rem auto;"
    , "    max-width: 500px;"
    , "    width: 90%;"
    , "    font-size: 2.5rem;"
    , "    font-weight: 900;"
    , "    text-transform: uppercase;"
    , "    letter-spacing: 0.05em;"
    , "    border-radius: 0;"
    , "    border-right: 2px solid #ffffff;"
    , "    border-bottom: 2px solid #ffffff;"
    , "    outline: 3px solid #000000;"
    , "    z-index: 10;"
    , "    box-sizing: border-box;"
    , "}"
    , ""
    , "/* Album Title Styles */"
    , ".album-title {"
    , "    text-align: center;"
    , "    margin-top: 1rem;"
    , "}"
    , ""
    , ".title-line {"
    , "    margin: 0.3rem 0;"
    , "    line-height: 1.2;"
    , "    color: #f5f5dc;"
    , "}"
    , ""
    , ".title-line-1 {"
    , "    font-style: italic;"
    , "    font-size: 1.1rem;"
    , "    color: #e6e6cc;"
    , "    letter-spacing: 0.05em;"
    , "}"
    , ""
    , ".title-line-2 {"
    , "    font-weight: bold;"
    , "    font-size: 2.2rem;"
    , "    color: #f5f5dc;"
    , "    letter-spacing: 0.1em;"
    , "    text-transform: uppercase;"
    , "}"
    , ""
    , ".title-line-3 {"
    , "    font-style: italic;"
    , "    font-family: 'Brush Script MT', 'Lucida Handwriting', cursive;"
    , "    font-size: 1.1rem;"
    , "    color: #e6e6cc;"
    , "    letter-spacing: 0.05em;"
    , "}"
    , ""
    , ".title-link {"
    , "    text-decoration: underline;"
    , "    color: #f5f5dc;"
    , "    transition: all 0.3s ease;"
    , "}"
    , ""
    , ".title-link:hover {"
    , "    color: #ff6347;"
    , "    text-decoration-color: #ff6347;"
    , "    text-shadow: 0 0 8px rgba(255, 99, 71, 0.4);"
    , "}"
    , ""

    , ""
    , ".album-artwork a {"
    , "    text-decoration: none;"
    , "    display: inline-block;"
    , "    transition: all 0.3s ease;"
    , "}"
    , ""
    , ".album-artwork a:hover {"
    , "    transform: scale(1.02);"
    , "}"
    , ""
    , ".navigation-arrow:hover {"
    , "    background: rgba(255, 255, 255, 0.2);"
    , "    color: #ffffff;"
    , "    transform: translateX(-50%) scale(1.05);"
    , "    border-color: rgba(255, 255, 255, 0.6);"
    , "    box-shadow: 0 4px 20px rgba(255, 255, 255, 0.2);"
    , "}"
    , ""
    , "@keyframes float {"
    , "    0%, 100% {"
    , "        transform: translateX(-50%) translateY(0px);"
    , "    }"
    , "    50% {"
    , "        transform: translateX(-50%) translateY(-10px);"
    , "    }"
    , "}"
    , ""
    , ".content-area {"
    , "    position: relative;"
    , "    z-index: 10;"
    , "    background: rgba(0, 0, 0, 0.6);"
    , "    padding: 3rem 2.5rem;"
    , "    border-radius: 0;"
    , "    backdrop-filter: blur(10px);"
    , "    border-right: 2px solid #fff;"
    , "    border-bottom: 2px solid #fff;"
    , "    max-width: 500px;"
    , "    width: 90%;"
    , "    box-sizing: border-box;"
    , "    margin-bottom: 2rem;"
    , "}"
    , ""
    , ".content-area h1 {"
    , "    font-size: 2.5rem;"
    , "    margin-bottom: 0;"
    , "    color: #f5f5dc;"
    , "    font-weight: bold;"
    , "    letter-spacing: 0.1em;"
    , "}"
    , ""
    , ".content-area p {"
    , "    font-size: 1.2rem;"
    , "    margin-bottom: 1rem;"
    , "    color: #e6e6cc;"
    , "}"
    , ""
    , "/* Coming Soon Card Styles */"
    , ".coming-soon-card {"
    , "    position: relative;"
    , "    cursor: pointer;"
    , "    transition: all 0.3s ease;"
    , "}"
    , ""
    , ".coming-soon-card:hover {"
    , "    transform: scale(1.02);"
    , "    box-shadow: 0 0 30px rgba(255, 255, 255, 0.2);"
    , "}"
    , ""
    , ".coming-soon-overlay {"
    , "    position: absolute;"
    , "    top: 0;"
    , "    left: 0;"
    , "    right: 0;"
    , "    bottom: 0;"
    , "    background: rgba(0, 0, 0, 0.8);"
    , "    color: #fff;"
    , "    display: flex;"
    , "    align-items: center;"
    , "    justify-content: center;"
    , "    font-size: 1.5rem;"
    , "    font-weight: bold;"
    , "    opacity: 0;"
    , "    transition: opacity 0.3s ease;"
    , "    border-radius: 0;"
    , "    backdrop-filter: blur(5px);"
    , "}"
    , ""
    , ".coming-soon-card:hover .coming-soon-overlay {"
    , "    opacity: 1;"
    , "}"
    , ""
    , ".title-text {"
    , "    color: #f5f5dc;"
    , "    text-decoration: none;"
    , "    transition: color 0.3s ease;"
    , "}"
    , ""
    , ".coming-soon-card:hover .title-text {"
    , "    color: #ccc;"
    , "}"

    ]

-- Serve components CSS
serveComponentsCSS :: LBS.ByteString
serveComponentsCSS = LBS.fromStrict $ encodeUtf8 $ T.pack $ unlines [
    "/* Components CSS */"
    , ""
    , "/* MenuBar Styles */"
    , ".main-header {"
    , "    position: fixed;"
    , "    top: 0;"
    , "    left: 0;"
    , "    right: 0;"
    , "    width: 100%;"
    , "    z-index: 1000;"
    , "    background: rgba(0, 0, 0, 0.95);"
    , "    backdrop-filter: blur(10px);"
    , "    border-bottom: 1px solid #333;"
    , "    box-shadow: 0 2px 10px rgba(0, 0, 0, 0.3);"
    , "}"
    , ""
    , ".main-nav {"
    , "    padding: 0;"
    , "}"
    , ""
    , ".nav-container {"
    , "    display: flex;"
    , "    justify-content: space-between;"
    , "    align-items: center;"
    , "    max-width: 1200px;"
    , "    margin: 0 auto;"
    , "    padding: 1rem 2rem;"
    , "}"
    , ""
    , ".brand-logo {"
    , "    display: flex;"
    , "    flex-direction: column;"
    , "    align-items: flex-start;"
    , "    text-decoration: none;"
    , "    color: #f5f5dc;"
    , "    padding: 15px;"
    , "    border-radius: 8px;"
    , "    transition: all 0.3s ease;"
    , "}"
    , ""
    , ".brand-text {"
    , "    font-size: 2rem;"
    , "    font-weight: bold;"
    , "    font-style: italic;"
    , "    margin-bottom: 0.2rem;"
    , "    color: #fff;"
    , "    text-decoration: none;"
    , "    transition: all 0.3s ease;"
    , "}"
    , ""
    , ".brand-logo:hover {"
    , "    box-shadow: 0 0 20px rgba(255, 99, 71, 0.3);"
    , "}"
    , ""
    , ".brand-text:hover {"
    , "    color: #ff6347;"
    , "    transform: scale(1.05);"
    , "}"
    , ""
    , ".brand-subtitle {"
    , "    font-size: 0.8rem;"
    , "    color: #ccc;"
    , "    letter-spacing: 0.1em;"
    , "    animation: pulse 2s ease-in-out infinite;"
    , "    transition: all 0.3s ease;"
    , "}"
    , ""
    , "@keyframes pulse {"
    , "    0%, 100% { opacity: 0.7; }"
    , "    50% { opacity: 1; }"
    , "}"
    , ""
    , ".nav-links {"
    , "    display: flex;"
    , "    list-style: none;"
    , "    margin: 0;"
    , "    padding: 0;"
    , "    gap: 2rem;"
    , "}"
    , ""
    , ".nav-item {"
    , "    position: relative;"
    , "}"
    , ""
    , ".nav-link {"
    , "    color: #ccc;"
    , "    text-decoration: none;"
    , "    font-size: 1rem;"
    , "    font-weight: 500;"
    , "    padding: 0.5rem 1rem;"
    , "    border-radius: 4px;"
    , "    transition: all 0.3s ease;"
    , "    position: relative;"
    , "}"
    , ""
    , ".nav-link:hover {"
    , "    color: #cc2e1f;"
    , "    background: rgba(204, 46, 31, 0.1);"
    , "    text-shadow: 0 0 5px rgba(204, 46, 31, 0.3);"
    , "    transform: translateY(-2px);"
    , "    box-shadow: 0 4px 12px rgba(204, 46, 31, 0.2);"
    , "}"
    , ""
    , ".nav-link.active {"
    , "    color: #ff6347;"
    , "    background: rgba(255, 99, 71, 0.15);"
    , "    text-shadow: 0 0 8px rgba(255, 99, 71, 0.4);"
    , "    border: 1px solid rgba(255, 99, 71, 0.3);"
    , "}"
    , ""
    , "/* Cart Icon Styles */"
    , ".nav-section {"
    , "    display: flex;"
    , "    align-items: center;"
    , "    gap: 2rem;"
    , "}"
    , ""
    , ".cart-icon-container {"
    , "    position: relative;"
    , "    display: flex;"
    , "    align-items: center;"
    , "}"
    , ""
    , ".cart-icon {"
    , "    display: flex;"
    , "    align-items: center;"
    , "    gap: 0.5rem;"
    , "    color: #fff;"
    , "    text-decoration: none;"
    , "    padding: 0.5rem 0.75rem;"
    , "    border-radius: 6px;"
    , "    transition: all 0.3s ease;"
    , "    position: relative;"
    , "}"
    , ""
    , ".cart-icon:hover {"
    , "    color: #ff6347;"
    , "    background: rgba(255, 99, 71, 0.1);"
    , "    transform: translateY(-2px);"
    , "    box-shadow: 0 4px 12px rgba(255, 99, 71, 0.2);"
    , "}"
    , ""
    , ".cart-icon.has-items {"
    , "    color: #ff6347;"
    , "}"
    , ""
    , ".cart-icon.has-items:hover {"
    , "    color: #cc2e1f;"
    , "    background: rgba(204, 46, 31, 0.1);"
    , "    box-shadow: 0 4px 12px rgba(204, 46, 31, 0.2);"
    , "}"
    , ""
    , ".cart-icon-symbol {"
    , "    font-size: 0.9rem;"
    , "    line-height: 1;"
    , "    font-weight: 600;"
    , "    letter-spacing: 0.1em;"
    , "    text-transform: uppercase;"
    , "}"
    , ""
    , ".cart-dropdown {"
    , "    position: absolute;"
    , "    top: 100%;"
    , "    right: 0;"
    , "    background: #111;"
    , "    border: 1px solid #333;"
    , "    border-radius: 8px;"
    , "    min-width: 250px;"
    , "    box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3);"
    , "    opacity: 0;"
    , "    visibility: hidden;"
    , "    transform: translateY(-10px);"
    , "    transition: all 0.3s ease;"
    , "    z-index: 1000;"
    , "    margin-top: 0.5rem;"
    , "}"
    , ""
    , ".cart-dropdown.open {"
    , "    opacity: 1;"
    , "    visibility: visible;"
    , "    transform: translateY(0);"
    , "}"
    , ""
    , ".cart-dropdown-content {"
    , "    padding: 1rem;"
    , "}"
    , ""
    , ".cart-empty-message {"
    , "    color: #ccc;"
    , "    text-align: center;"
    , "    font-size: 0.9rem;"
    , "    margin-bottom: 1rem;"
    , "    font-style: italic;"
    , "}"
    , ""
    , ".cart-close-btn {"
    , "    width: 100%;"
    , "    background: #ff6347;"
    , "    color: #fff;"
    , "    border: none;"
    , "    padding: 0.5rem 1rem;"
    , "    border-radius: 4px;"
    , "    font-size: 0.8rem;"
    , "    font-weight: 600;"
    , "    cursor: pointer;"
    , "    transition: all 0.3s ease;"
    , "}"
    , ""
    , ".cart-close-btn:hover {"
    , "    background: #cc2e1f;"
    , "    transform: translateY(-1px);"
    , "}"
    , ""
    , ".mobile-menu-toggle {"
    , "    display: none;"
    , "    flex-direction: column;"
    , "    background: none;"
    , "    border: none;"
    , "    cursor: pointer;"
    , "    padding: 0.5rem;"
    , "    margin-left: auto;"
    , "    transition: all 0.3s ease;"
    , "}"
    , ""
    , ".mobile-menu-toggle:hover .hamburger-line {"
    , "    background: #cc2e1f;"
    , "    box-shadow: 0 0 5px rgba(204, 46, 31, 0.3);"
    , "}"
    , ""
    , ".mobile-menu-toggle.active .hamburger-line {"
    , "    background: #ff6347;"
    , "    box-shadow: 0 0 8px rgba(255, 99, 71, 0.4);"
    , "}"
    , ""
    , ".hamburger-line {"
    , "    width: 25px;"
    , "    height: 3px;"
    , "    background: #fff;"
    , "    margin: 3px 0;"
    , "    transition: 0.3s;"
    , "    border-radius: 2px;"
    , "}"
    , ""
    , "/* Footer Styles */"
    , ".main-footer {"
    , "    background: #111;"
    , "    color: #ccc;"
    , "    padding: 1.5rem 0 1rem;"
    , "    border-top: 1px solid #333;"
    , "    position: relative;"
    , "    z-index: 10;"
    , "    margin-top: auto;"
    , "    flex-shrink: 0;"
    , "}"
    , ""
    , ".footer-content {"
    , "    max-width: 1200px;"
    , "    margin: 0 auto;"
    , "    padding: 0 2rem;"
    , "}"
    , ""
    , ".footer-main {"
    , "    margin-bottom: 1rem;"
    , "}"
    , ""
    , ".footer-brand {"
    , "    margin-bottom: 0.5rem;"
    , "}"
    , ""
    , ".footer-description {"
    , "    color: #999;"
    , "    font-size: 0.9rem;"
    , "    line-height: 1.4;"
    , "    max-width: 600px;"
    , "}"
    , ""

    , ""
    , ".footer-bottom {"
    , "    border-top: 1px solid #333;"
    , "    padding-top: 1rem;"
    , "    padding-bottom: 1rem;"
    , "    display: flex;"
    , "    justify-content: space-between;"
    , "    align-items: center;"
    , "    flex-wrap: wrap;"
    , "    gap: 1rem;"
    , "}"
    , ""
    , ".footer-copyright {"
    , "    color: #999;"
    , "    font-size: 0.9rem;"
    , "}"
    , ""
    , ".footer-legal {"
    , "    display: flex;"
    , "    gap: 1rem;"
    , "    align-items: center;"
    , "}"
    , ""
    , ".footer-legal a {"
    , "    color: #ccc;"
    , "    text-decoration: none;"
    , "    font-size: 0.9rem;"
    , "    transition: color 0.3s ease;"
    , "}"
    , ""
    , ".footer-legal a:hover {"
    , "    color: #fff;"
    , "}"
    , ""
    , ".separator {"
    , "    color: #666;"
    , "}"
    ]

-- Serve responsive CSS
serveResponsiveCSS :: LBS.ByteString
serveResponsiveCSS = LBS.fromStrict $ encodeUtf8 $ T.pack $ unlines [
    "/* Responsive CSS */"
    , ""
    , "/* Mobile MenuBar Styles */"
    , "@media (max-width: 768px) {"
    , "    .nav-container {"
    , "        padding: 0.8rem 1rem;"
    , "    }"
    , ""
    , "    .brand-text {"
    , "        font-size: 1.5rem;"
    , "    }"
    , ""
    , "    .brand-subtitle {"
    , "        font-size: 0.7rem;"
    , "    }"
    , ""
    , "    .nav-links {"
    , "        display: none;"
    , "        position: absolute;"
    , "        top: 100%;"
    , "        left: 0;"
    , "        right: 0;"
    , "        background: rgba(0, 0, 0, 0.98);"
    , "        flex-direction: column;"
    , "        padding: 1rem;"
    , "        border-top: 1px solid #333;"
    , "        gap: 0;"
    , "        animation: slideDown 0.3s ease-out;"
    , "    }"
    , ""
    , "    .nav-links.active {"
    , "        display: flex;"
    , "    }"
    , ""
    , "    .nav-item {"
    , "        width: 100%;"
    , "    }"
    , ""
    , "    .nav-link {"
    , "        display: block;"
    , "        padding: 1rem;"
    , "        width: 100%;"
    , "        text-align: center;"
    , "        border-bottom: 1px solid #333;"
    , "    }"
    , ""
    , "    .nav-link:last-child {"
    , "        border-bottom: none;"
    , "    }"
    , ""
    , "    .mobile-menu-toggle {"
    , "        display: flex;"
    , "    }"
    , ""
    , "    .mobile-menu-toggle.active .hamburger-line:nth-child(1) {"
    , "        transform: rotate(-45deg) translate(-5px, 6px);"
    , "    }"
    , ""
    , "    .mobile-menu-toggle.active .hamburger-line:nth-child(2) {"
    , "        opacity: 0;"
    , "    }"
    , ""
    , "    .mobile-menu-toggle.active .hamburger-line:nth-child(3) {"
    , "        transform: rotate(45deg) translate(-5px, -6px);"
    , "    }"
    , ""
    , "    @keyframes slideDown {"
    , "        from {"
    , "            opacity: 0;"
    , "            transform: translateY(-10px);"
    , "        }"
    , "        to {"
    , "            opacity: 1;"
    , "            transform: translateY(0);"
    , "        }"
    , "    }"
    , "}"
    , ""
    , "/* Footer Responsive Styles */"
    , "@media (max-width: 768px) {"
    , "    .footer-main {"
    , "        text-align: center;"
    , "        margin-bottom: 0.5rem;"
    , "    }"
    , ""
    , "    .footer-bottom {"
    , "        flex-direction: column;"
    , "        text-align: center;"
    , "        gap: 0.5rem;"
    , "        padding-bottom: 0.8rem;"
    , "    }"
    , ""
    , "    .main-footer {"
    , "        padding: 1rem 0 0.5rem;"
    , "    }"
    , ""
    , "    .footer-description {"
    , "        font-size: 0.8rem;"
    , "    }"
    , "}"
    , ""
    , "/* Small mobile adjustments */"
    , "@media (max-width: 480px) {"
    , "    .nav-container {"
    , "        padding: 0.5rem 0.8rem;"
    , "    }"
    , ""
    , "    .brand-text {"
    , "        font-size: 1.3rem;"
    , "    }"
    , ""
    , "    .footer-content {"
    , "        padding: 0 1rem;"
    , "    }"
    , ""
    , "    .main-footer {"
    , "        padding: 0.8rem 0 1rem;"
    , "    }"
    , "}"
    ]
