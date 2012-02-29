{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Iteratee
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee, Enumeratee)
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO as I
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Data.ZoomCache.Codec
import System.Environment (getArgs)

----------------------------------------------------------------------

data HPHeader = HPHeader
    { hpJob :: ByteString
    , hpDate :: ByteString
    , hpSampleUnit :: ByteString
    , hpValueUnit :: ByteString
    }
    deriving (Show, Typeable)

data HeapProfile = HeapProfile
    { hpHeader :: HPHeader
    , hpSampleStart :: Maybe Double
    , hpSampleEnd :: Maybe Double
    , hpSamples :: Map ByteString Int
    }
    deriving (Show, Typeable)

trackTypeHeapProfile :: ByteString
trackTypeHeapProfile = "ZOOMheap"

instance ZoomReadable HeapProfile where
    data SummaryData HeapProfile = SummaryHeapProfile
        { summaryHPHeader :: Maybe HPHeader
        , summaryHPSampleTimes :: [(Double, Double)]
        , summaryHPSamples :: Map ByteString Int
        } deriving (Show)

    trackIdentifier = const trackTypeHeapProfile

    readRaw = undefined
    readSummary = undefined

    prettyRaw = show
    prettySummaryData = show

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

    fromRaw = undefined
    fromSummaryData = undefined

    initSummaryWork = initSummaryHeapProfile
    toSummaryData   = mkSummaryHeapProfile
    updateSummaryData = updateSummaryHeapProfile
    appendSummaryData = appendSummaryHeapProfile

initSummaryHeapProfile :: SampleOffset -> SummaryWork HeapProfile
initSummaryHeapProfile = const $ SummaryWorkHeapProfile Nothing [] Map.empty

mkSummaryHeapProfile :: SampleOffsetDiff -> SummaryWork HeapProfile -> SummaryData HeapProfile
mkSummaryHeapProfile (SODiff dur) SummaryWorkHeapProfile{..} = SummaryHeapProfile
    { summaryHPHeader = swHPHeader
    , summaryHPSampleTimes = swHPSampleTimes
    , summaryHPSamples = swHPSamples
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
    }

----------------------------------------------------------------------

emptyHeapProfile :: HeapProfile
emptyHeapProfile = HeapProfile emptyHPHeader Nothing Nothing Map.empty

emptyHPHeader :: HPHeader
emptyHPHeader = HPHeader "" "" "" ""

clearHeapProfile :: HeapProfile -> HeapProfile
clearHeapProfile hp = hp { hpSampleStart = Nothing, hpSampleEnd = Nothing, hpSamples = Map.empty }

main :: IO ()
main = do
    mapM_ hpFile =<< getArgs

hpFile :: FilePath -> IO ()
hpFile path = do
    c <- I.run =<< I.enumFileRandom 102400 path (I.joinI $ (hpEnum (emptyHeapProfile) hpDo))
    putStrLn $ "Got " ++ show c
    return ()

hpDo :: MonadIO m => Iteratee [HeapProfile] m ()
hpDo = do
    ps <- I.head
    liftIO $ putStrLn $ "Got " ++ show ps

    ps' <- I.head
    liftIO $ putStrLn $ "Got " ++ show ps'

    ps'' <- I.head
    liftIO $ putStrLn $ "Got " ++ show ps''

    ps''' <- I.head
    liftIO $ putStrLn $ "Got " ++ show ps'''

    ps'''' <- I.head
    liftIO $ putStrLn $ "Got " ++ show ps''''

    ps''''' <- I.head
    liftIO $ putStrLn $ "Got " ++ show ps'''''

hpEnum :: HeapProfile -> Enumeratee ByteString [HeapProfile] IO a
hpEnum = I.unfoldConvStream hpIter

hpIter :: HeapProfile -> Iteratee ByteString IO (HeapProfile, [HeapProfile])
hpIter = parserToIteratee . hpParse

hpParse :: HeapProfile -> Parser (HeapProfile, [HeapProfile])
hpParse hp = do
    hp' <- parseJob hp <|> parseDate hp <|> parseSampleUnit hp <|> parseValueUnit hp <|> parseSample hp
    return (hp', [hp'])

labelledString :: ByteString -> Parser ByteString
labelledString l = do
    string l
    skipSpace
    char '\"'
    t <- takeTill (== '\"')
    char '\"'
    endOfLine
    return t

labelledDouble :: ByteString -> Parser Double
labelledDouble l = do
    string l
    skipSpace
    d <- double
    endOfLine
    return d

parseJob :: HeapProfile -> Parser HeapProfile
parseJob hp = do
    t <- labelledString "JOB"
    let hdr = (hpHeader hp) { hpJob = t }
    return hp{hpHeader = hdr}
    
parseDate :: HeapProfile -> Parser HeapProfile
parseDate hp = do
    t <- labelledString "DATE"
    let hdr = (hpHeader hp) { hpDate = t }
    return hp{hpHeader = hdr}

parseSampleUnit :: HeapProfile -> Parser HeapProfile
parseSampleUnit hp = do
    t <- labelledString "SAMPLE_UNIT"
    let hdr = (hpHeader hp) { hpSampleUnit = t }
    return hp{hpHeader = hdr}

parseValueUnit :: HeapProfile -> Parser HeapProfile
parseValueUnit hp = do
    t <- labelledString "VALUE_UNIT"
    let hdr = (hpHeader hp) { hpValueUnit = t }
    return hp{hpHeader = hdr}

parseSample :: HeapProfile -> Parser HeapProfile
parseSample hp = do
    hp' <- parseSampleStart (clearHeapProfile hp)
    hp'' <- parseSampleValues hp'
    parseSampleEnd hp''

parseSampleStart :: HeapProfile -> Parser HeapProfile
parseSampleStart hp = do
    d <- labelledDouble "BEGIN_SAMPLE"
    return hp{hpSampleStart = Just d}

parseSampleEnd :: HeapProfile -> Parser HeapProfile
parseSampleEnd hp = do
    d <- labelledDouble "END_SAMPLE"
    return hp{hpSampleEnd = Just d}

parseSampleValues :: HeapProfile -> Parser HeapProfile
parseSampleValues hp = do
    vs <- many parseSampleValue
    return hp { hpSamples = Map.fromList vs }

parseSampleValue :: Parser (ByteString, Int)
parseSampleValue = do
    n <- takeTill isSpace
    skipSpace
    d <- decimal
    endOfLine
    return (n, d)

