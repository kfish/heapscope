{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Main (main) where

import Control.Monad.Trans (lift, MonadIO)
import Data.Attoparsec.Iteratee
import Data.ByteString (ByteString)
import Data.Default
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee, Enumeratee)
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO as I
import Data.ZoomCache
import System.Environment (getArgs)

import HeapScope.HeapProfile
import HeapScope.Parse
import HeapScope.ZoomCache ()

----------------------------------------------------------------------

main :: IO ()
main = do
    mapM_ hpFile =<< getArgs

hpFile :: FilePath -> IO ()
hpFile path = withFileWrite trackMap Nothing True iter zpath
    where
        iter = I.run =<< I.enumFileRandom 102400 path (I.joinI $ (hpEnum (emptyHeapProfile) hpDo))
        zpath = path ++ ".zoom"
        trackMap = IM.singleton 1 (setCodec (undefined :: HeapProfile) spec)
        spec = def { specName = "hp" }

hpDo :: Iteratee [HeapProfile] ZoomW ()
hpDo = do
    lift $ setWatermark 1 2
    lift . write 1 =<< I.head
    lift . write 1 =<< I.head
    lift . write 1 =<< I.head

hpEnum :: MonadIO m => HeapProfile -> Enumeratee ByteString [HeapProfile] m a
hpEnum = I.unfoldConvStream hpIter

hpIter :: MonadIO m => HeapProfile -> Iteratee ByteString m (HeapProfile, [HeapProfile])
hpIter = parserToIteratee . hpParse

