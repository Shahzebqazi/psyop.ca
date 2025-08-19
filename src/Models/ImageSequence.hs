{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.ImageSequence
    ( ImageIndex(..)
    , SequenceSize(..)
    , ImageSequence(..)
    , mkImageSequence
    , mkSequenceSize
    , nextImage
    , getCurrentImage
    , testNoRepetition
    ) where

-- Pure data models for image sequence management
data ImageIndex = ImageIndex Int deriving (Show, Eq, Ord)
data SequenceSize = SequenceSize Int deriving (Show, Eq, Ord)

data ImageSequence = ImageSequence
    { currentIndex :: ImageIndex
    , totalImages :: SequenceSize
    , usedCount :: Int
    } deriving (Show, Eq)

-- Smart constructors with validation
mkSequenceSize :: Int -> Maybe SequenceSize
mkSequenceSize n | n > 0 && n <= 1000 = Just (SequenceSize n)
                 | otherwise = Nothing

mkImageSequence :: SequenceSize -> ImageSequence
mkImageSequence seqSize = ImageSequence (ImageIndex 0) seqSize 0

-- Pure functions for image sequence logic
nextImage :: ImageSequence -> ImageSequence
nextImage imgSeq = imgSeq 
    { currentIndex = ImageIndex ((unImageIndex (currentIndex imgSeq) + 1) `mod` unSequenceSize (totalImages imgSeq))
    , usedCount = usedCount imgSeq + 1
    }
  where
    unImageIndex (ImageIndex idx) = idx
    unSequenceSize (SequenceSize sz) = sz

getCurrentImage :: ImageSequence -> ImageIndex
getCurrentImage = currentIndex

-- Test function for sequence validation
testNoRepetition :: Int -> Bool
testNoRepetition n = case mkSequenceSize n of
    Just seqSize -> let imgSeq = mkImageSequence seqSize
                        finalSeq = iterate nextImage imgSeq !! n
                    in usedCount finalSeq == n
    Nothing -> False
