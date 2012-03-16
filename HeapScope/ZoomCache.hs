{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module HeapScope.ZoomCache (
) where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.Foldable as Fold
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.ZoomCache.Codec

import HeapScope.HeapProfile

----------------------------------------------------------------------

trackTypeHeapProfile :: ByteString
trackTypeHeapProfile = "ZOOMheap"

instance ZoomReadable HeapProfile where
    data SummaryData HeapProfile = SummaryHeapProfile
        { summaryHPHeader :: Maybe HPHeader
        , summaryHPSampleTimes :: [(Double, Double)]
        , summaryHPSamples :: Map ByteString Int
        , summaryHPTotal :: Integer
        } deriving (Show)

    trackIdentifier = const trackTypeHeapProfile

    readRaw = readHeapProfile
    readSummary = readSummaryHeapProfile

    prettyRaw = show
    prettySummaryData = show

readByteString0 :: (Functor m, Monad m)
                => Iteratee ByteString m ByteString
readByteString0 = I.takeWhile (/= 0)

readHPHeader :: (Functor m, Monad m)
             => Iteratee ByteString m HPHeader
readHPHeader = do
    hpJob <- readByteString0
    hpDate <- readByteString0
    hpSampleUnit <- readByteString0
    hpValueUnit <- readByteString0
    return HPHeader{..}

iterReadMaybe :: (Functor m, Monad m)
          => Iteratee ByteString m a
          -> Iteratee ByteString m (Maybe a)
iterReadMaybe iter = do
    m <- I.head
    case m of
        0 -> return Nothing
        _ -> Just <$> iter

iterReadTuple :: (Functor m, Monad m)
          => Iteratee ByteString m a
          -> Iteratee ByteString m b
          -> Iteratee ByteString m (a, b)
iterReadTuple iterA iterB = do
    a <- iterA
    b <- iterB
    return (a, b)

iterReadList :: (Functor m, Monad m)
         => Iteratee ByteString m a
         -> Iteratee ByteString m [a]
iterReadList iter = do
    n <- fromIntegral <$> readIntegerVLC
    replicateM n iter

iterReadMap :: (Functor m, Monad m, Ord a)
        => Iteratee ByteString m a
        -> Iteratee ByteString m b
        -> Iteratee ByteString m (Map a b)
iterReadMap iterA iterB = Map.fromList <$> iterReadList (iterReadTuple iterA iterB)

readSampleTimes :: (Functor m, Monad m)
                => Iteratee ByteString m [(Double, Double)]
readSampleTimes = iterReadList (iterReadTuple readDouble64be readDouble64be)

readSamples :: (Functor m, Monad m)
            => Iteratee ByteString m (Map ByteString Int)
readSamples = iterReadMap readByteString0 (fromIntegral <$> readIntegerVLC)

readHeapProfile :: (Functor m, Monad m)
                => Iteratee ByteString m HeapProfile
readHeapProfile = do
    hpHeader <- readHPHeader
    hpSampleStart <- iterReadMaybe readDouble64be
    hpSampleEnd <- iterReadMaybe readDouble64be
    hpSamples <- readSamples
    return HeapProfile{..}

readSummaryHeapProfile :: (Functor m, Monad m)
                       => Iteratee ByteString m (SummaryData HeapProfile)
readSummaryHeapProfile = do
    summaryHPHeader <- iterReadMaybe readHPHeader
    summaryHPSampleTimes <- readSampleTimes
    summaryHPSamples <- readSamples
    summaryHPTotal <- readIntegerVLC
    return SummaryHeapProfile{..}

instance ZoomWrite HeapProfile where
    write = writeData

instance ZoomWrite (SampleOffset, HeapProfile) where
    write = writeDataVBR

instance ZoomWrite (TimeStamp, HeapProfile) where
    write = writeDataTS

instance ZoomWritable HeapProfile where
    data SummaryWork HeapProfile = SummaryWorkHeapProfile
        { swHPHeader :: Maybe HPHeader
        , swHPSampleTimes :: [(Double, Double)]
        , swHPSamples :: Map ByteString Int
        }

    fromRaw = fromHeapProfile
    fromSummaryData = fromSummaryHeapProfile

    initSummaryWork = initSummaryHeapProfile
    toSummaryData   = mkSummaryHeapProfile
    updateSummaryData = updateSummaryHeapProfile
    appendSummaryData = appendSummaryHeapProfile

buildMaybe :: (a -> Builder) -> Maybe a -> Builder
buildMaybe _     Nothing = fromIntegerVLC 0
buildMaybe build (Just x) = fromIntegerVLC 1 `mappend` build x

buildTuple :: (a -> Builder) -> (b -> Builder) -> (a, b) -> Builder
buildTuple buildA buildB (a, b) = buildA a `mappend` buildB b

buildList :: (a -> Builder) -> [a] -> Builder
buildList build xs = mconcat $
    (fromIntegerVLC . fromIntegral . length $ xs) : map build xs

buildMap :: (a -> Builder) -> (b -> Builder) -> Map a b -> Builder
buildMap buildA buildB m = buildList (buildTuple buildA buildB) (Map.assocs m)

-- | Build a NUL-terminated ByteString
fromByteString0 :: ByteString -> Builder
fromByteString0 bs = fromByteString bs `mappend` fromWord8 0

fromSampleTimes :: [(Double, Double)] -> Builder
fromSampleTimes = buildList (buildTuple fromDouble fromDouble)

fromSamples :: Map ByteString Int -> Builder
fromSamples = buildMap fromByteString0 (fromIntegerVLC . fromIntegral)

fromHPHeader :: HPHeader -> Builder
fromHPHeader HPHeader{..} = mconcat $ map fromByteString0
    [hpJob, hpDate, hpSampleUnit, hpValueUnit]

fromHeapProfile :: HeapProfile -> Builder
fromHeapProfile HeapProfile{..} = mconcat
    [ fromHPHeader hpHeader
    , buildMaybe fromDouble hpSampleStart
    , buildMaybe fromDouble hpSampleEnd
    , fromSamples hpSamples
    ]

fromSummaryHeapProfile :: SummaryData HeapProfile -> Builder
fromSummaryHeapProfile SummaryHeapProfile{..} = mconcat
    [ buildMaybe fromHPHeader summaryHPHeader
    , fromSampleTimes summaryHPSampleTimes
    , fromSamples summaryHPSamples
    , fromIntegerVLC summaryHPTotal
    ]

initSummaryHeapProfile :: SampleOffset -> SummaryWork HeapProfile
initSummaryHeapProfile = const $ SummaryWorkHeapProfile Nothing [] Map.empty

mkSummaryHeapProfile :: SampleOffsetDiff -> SummaryWork HeapProfile -> SummaryData HeapProfile
mkSummaryHeapProfile (SODiff dur) SummaryWorkHeapProfile{..} = SummaryHeapProfile
    { summaryHPHeader = swHPHeader
    , summaryHPSampleTimes = swHPSampleTimes
    , summaryHPSamples = swHPSamples
    , summaryHPTotal = fromIntegral (Fold.sum swHPSamples)
    }

updateSummaryHeapProfile :: SampleOffset -> HeapProfile -> SummaryWork HeapProfile
                         -> SummaryWork HeapProfile
updateSummaryHeapProfile t HeapProfile{..} sw@SummaryWorkHeapProfile{..} =
    sw { swHPHeader = Just hpHeader
       , swHPSampleTimes = times
       , swHPSamples = Map.unionWith (+) swHPSamples hpSamples
       }
    where
        times = case (hpSampleStart, hpSampleEnd) of
            (Just s, Just e) -> swHPSampleTimes ++ [(s, e)]
            _                -> swHPSampleTimes

appendSummaryHeapProfile :: SampleOffsetDiff -> SummaryData HeapProfile
                         -> SampleOffsetDiff -> SummaryData HeapProfile
                         -> SummaryData HeapProfile
appendSummaryHeapProfile (SODiff dur1) s1 (SODiff dur2) s2 = SummaryHeapProfile
    { summaryHPHeader = summaryHPHeader s1
    , summaryHPSampleTimes = summaryHPSampleTimes s1 ++ summaryHPSampleTimes s2
    , summaryHPSamples = Map.unionWith (+) (summaryHPSamples s1) (summaryHPSamples s2)
    , summaryHPTotal = (summaryHPTotal s1) + (summaryHPTotal s2)
    }

