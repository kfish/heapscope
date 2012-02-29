{-# LANGUAGE OverloadedStrings #-}
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
import System.Environment (getArgs)

data HeapProfile = HeapProfile
    { hpJob :: ByteString
    , hpDate :: ByteString
    , hpSampleUnit :: ByteString
    , hpValueUnit :: ByteString
    , hpSampleStart :: Maybe Double
    , hpSampleEnd :: Maybe Double
    , hpSamples :: Map ByteString Int
    }
    deriving (Show)

emptyHeapProfile :: HeapProfile
emptyHeapProfile = HeapProfile "" "" "" "" Nothing Nothing Map.empty

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
    return hp{hpJob = t}
    
parseDate :: HeapProfile -> Parser HeapProfile
parseDate hp = do
    t <- labelledString "DATE"
    return hp{hpDate = t}

parseSampleUnit :: HeapProfile -> Parser HeapProfile
parseSampleUnit hp = do
    t <- labelledString "SAMPLE_UNIT"
    return hp{hpSampleUnit = t}

parseValueUnit :: HeapProfile -> Parser HeapProfile
parseValueUnit hp = do
    t <- labelledString "VALUE_UNIT"
    return hp{hpValueUnit = t}

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

