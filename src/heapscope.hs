{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Main (main) where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Iteratee
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee, Enumeratee)
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO as I
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
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
    ]

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

