{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module HeapScope.Parse (
  -- * Functions
  hpParse
) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.Map as Map

import HeapScope.HeapProfile

hpParse :: HeapProfile -> Parser (HeapProfile, [HeapProfile])
hpParse hp = do
    hp' <- parseHeader hp <|> parseSample hp
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

parseHeader :: HeapProfile -> Parser HeapProfile
parseHeader hp = parseJob hp >>= parseDate >>= parseSampleUnit >>= parseValueUnit

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

