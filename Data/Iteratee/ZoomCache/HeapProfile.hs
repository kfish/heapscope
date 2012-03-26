{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Data.Iteratee.ZoomCache.HeapProfile (
      extentsHeapProfile

    , wholeTrackSummaryListHeapProfile
    , enumListHeapProfile
    , enumSummaryListHeapProfile
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import qualified Data.Iteratee as I
import Data.Iteratee.ZoomCache
import Data.Maybe
import Data.Offset
import Data.Typeable
import Data.TypeLevel.Num hiding ((==))
import Data.ZoomCache
import Data.ZoomCache.Multichannel.NList
import Data.ZoomCache.NList

import Scope.Types

import HeapScope.HeapProfile
import HeapScope.ZoomCache

----------------------------------------------------------------------

extentsHeapProfile :: (Functor m, MonadIO m)
                   => TrackNo -> I.Iteratee [Offset Block] m LayerExtents
extentsHeapProfile trackNo = sdToExtents <$> wholeTrackSummaryListHeapProfile trackNo
    where
        sdToExtents :: [Summary HeapProfile] -> LayerExtents
        sdToExtents ss = LayerExtents entry exit (maxRange ss)
            where
                s = head ss
                entry = summaryEntry s
                exit = summaryExit s

        maxRange :: [Summary HeapProfile] -> Double
        maxRange = maximum . map yRange

        yRange :: Summary HeapProfile -> Double
        yRange = abs . fromIntegral . summaryHPTotal . summaryData

----------------------------------------------------------------------

rawToHP :: ZoomRaw -> [HeapProfile]
rawToHP (ZoomRaw xs) | typeOf xs == typeOf (undefined :: [HeapProfile]) =
                          fromMaybe [] (cast xs :: Maybe [HeapProfile])
                     | otherwise = []

rawToListHP :: ZoomRaw -> [[HeapProfile]]
rawToListHP (ZoomRaw xs) | not (null d) = [d]
                         | typeOf xs == typeOf (undefined :: [NList D1 HeapProfile]) =
                                         l (cast xs :: Maybe [NList D1 HeapProfile])
                         | otherwise = []
    where
        d = rawToHP (ZoomRaw xs)
        l :: Maybe [NList D1 a] -> [[a]]
        l = maybe [] (map nListToList)

toSummaryHP :: Typeable a => Summary a -> Maybe (Summary HeapProfile)
toSummaryHP s | typeOf s == typeOf (undefined :: Summary HeapProfile) =
                            id (cast s :: Maybe (Summary HeapProfile))
              | otherwise = Nothing

toSummaryListHP :: Typeable a => Summary a -> Maybe [Summary HeapProfile]
toSummaryListHP s | isJust sd = (:[]) <$> sd
                  | typeOf s == typeOf (undefined :: Summary (NList D1 HeapProfile)) =
                            sl <$> (cast s :: Maybe (Summary (NList D1 HeapProfile)))
                  | otherwise = Nothing
    where
        sd = toSummaryHP s
        sl :: Summary (NList D1 a) -> [Summary a]
        sl = summaryNListToList

----------------------------------------------------------------------

-- | Read the summary of an entire track.
wholeTrackSummaryListHeapProfile :: (Functor m, MonadIO m)
                                 => TrackNo
                                 -> I.Iteratee [Offset Block] m [Summary HeapProfile]
wholeTrackSummaryListHeapProfile trackNo =
    I.joinI $ filterTracks [trackNo] .  I.joinI . e $ I.last
    where
        e = I.joinI . enumSummaries . I.mapChunks (catMaybes . map toSLHP)
        toSLHP :: ZoomSummary -> Maybe [Summary HeapProfile]
        toSLHP (ZoomSummary s) = toSummaryListHP s

enumListHeapProfile :: (Functor m, Monad m)
                    => I.Enumeratee [Offset Block] [(TimeStamp, [HeapProfile])] m a
enumListHeapProfile = I.joinI . enumPackets . I.mapChunks (concatMap f)
    where
        f :: Packet -> [(TimeStamp, [HeapProfile])]
        f Packet{..} = zip packetTimeStamps (rawToListHP packetData)

enumSummaryListHeapProfile :: (Functor m, Monad m)
                           => Int
                           -> I.Enumeratee [Offset Block] [[Summary HeapProfile]] m a
enumSummaryListHeapProfile level =
    I.joinI . enumSummaryLevel level .
    I.mapChunks (catMaybes . map toSLHP)
    where
        toSLHP :: ZoomSummary -> Maybe [Summary HeapProfile]
        toSLHP (ZoomSummary s) = toSummaryListHP s

