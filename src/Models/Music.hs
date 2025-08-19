module Models.Music
    ( MusicTrack(..)
    , TrackStatus(..)
    , mkMusicTrack
    , getAvailableTracks
    , getTrackBySlug
    ) where

import Data.Text (Text)
import qualified Data.Text as T

-- Music track data model
data TrackStatus = ComingSoon | Available | Released deriving (Show, Eq, Ord)

data MusicTrack = MusicTrack
    { trackSlug :: Text
    , trackTitle :: Text
    , trackArtist :: Text
    , trackReleaseDate :: Maybe Text  -- Changed from Day to Text to avoid time dependency
    , trackStatus :: TrackStatus
    , trackArtwork :: Text
    , trackStreamingLinks :: [Text]
    , trackDescription :: Maybe Text
    } deriving (Show, Eq)

-- Smart constructor for music tracks
mkMusicTrack :: Text -> Text -> Text -> TrackStatus -> Text -> MusicTrack
mkMusicTrack slug title artist status artwork = MusicTrack
    { trackSlug = slug
    , trackTitle = title
    , trackArtist = artist
    , trackReleaseDate = Nothing
    , trackStatus = status
    , trackArtwork = artwork
    , trackStreamingLinks = []
    , trackDescription = Nothing
    }

-- Sample music data
sampleTracks :: [MusicTrack]
sampleTracks =
    [ mkMusicTrack 
        (T.pack "moonlight-paradox")
        (T.pack "psyop 01 Moonlight Paradox")
        (T.pack "PSYOP")
        Available 
        (T.pack "/assets/white/promo_5.jpg")
    , mkMusicTrack 
        (T.pack "great-in-vain")
        (T.pack "psyop 02 Great in Vain")
        (T.pack "PSYOP")
        ComingSoon 
        (T.pack "/assets/white/promo_1.jpg")
    ]

-- Get all available tracks
getAvailableTracks :: [MusicTrack]
getAvailableTracks = sampleTracks

-- Get track by slug
getTrackBySlug :: Text -> Maybe MusicTrack
getTrackBySlug slug = find (\track -> trackSlug track == slug) sampleTracks
  where
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs
