{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Main (main) where

import Control.Monad (foldM)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.Iteratee
import Data.ByteString (ByteString)
import Data.Default
import qualified Data.IntMap as IM
import Data.Iteratee (Iteratee, Enumeratee)
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO as I
import Data.ZoomCache
import System.Console.GetOpt
import UI.Command

import HeapScope.HeapProfile
import HeapScope.Parse
import HeapScope.ZoomCache ()

------------------------------------------------------------

data Config = Config
    { noRaw    :: Bool
    , wmLevel  :: Int
    , track    :: TrackNo
    , spec     :: TrackSpec
    }

instance Default Config where
    def = defConfig

defConfig :: Config
defConfig = Config
    { noRaw    = False
    , wmLevel  = 1024
    , track    = 1
    , spec     = def { specDeltaEncode = False
                     , specZlibCompress = False
                     , specName = "hp"
                     }
    }

data Option = NoRaw
            | Watermark String
            | Track String
            | ZLib
            | Label String
    deriving (Eq)

options :: [OptDescr Option]
options = genOptions

genOptions :: [OptDescr Option]
genOptions =
    [ Option ['z'] ["no-raw"] (NoArg NoRaw)
             "Do NOT include raw data in the output"
    , Option ['w'] ["watermark"] (ReqArg Watermark "watermark")
             "Set high-watermark level"
    , Option ['t'] ["track"] (ReqArg Track "trackNo")
             "Set or select track number"
    , Option ['Z'] ["zlib"] (NoArg ZLib)
             "Zlib-compress data"
    , Option ['l'] ["label"] (ReqArg Label "label")
             "Set track label"
    ]

processArgs :: [String] -> IO (Config, [String])
processArgs args = do
    case getOpt RequireOrder options args of
        (opts, args', [] ) -> do
            config <- processConfig def opts
            return (config, args')
        (_,    _,     _:_) -> return (def, args)

processConfig :: Config -> [Option] -> IO Config
processConfig = foldM processOneOption
    where
        processOneOption config NoRaw = do
            return $ config {noRaw = True}
        processOneOption config (Watermark s) = do
            return $ config {wmLevel = read s}
        processOneOption config (Track s) = do
            return $ config {track = read s}

        processOneOption config ZLib = do
            return $ config { spec = (spec config){specZlibCompress = True} }

        processOneOption config (Label s) = do
            return $ config { spec = (spec config){specName = C.pack s} }

----------------------------------------------------------------------

hszcGen :: Command ()
hszcGen = defCmd {
          cmdName = "gen"
        , cmdHandler = hszcGenHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate heapscope zoom-cache data"
        , cmdExamples = [("Read foo.hp, generating a file called foo.hp.zoom", "foo.hp")]
        }

hszcGenHandler :: App () ()
hszcGenHandler = do
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ mapM_ (hszcWriteFile config) filenames

hszcWriteFile :: Config -> FilePath -> IO ()
hszcWriteFile Config{..} path = withFileWrite trackMap Nothing True iter zpath
    where
        iter = I.run =<< I.enumFileRandom 102400 path (I.joinI $ (hpEnum (emptyHeapProfile) hpDo))
        zpath = path ++ ".zoom"
        trackMap = IM.singleton track (setCodec (undefined :: HeapProfile) spec)

hpDo :: Iteratee [HeapProfile] ZoomW ()
hpDo = I.mapM_ (\hp -> maybe (return ()) (\ts -> write 1 (TS ts, hp)) (hpSampleStart hp))

hpEnum :: MonadIO m => HeapProfile -> Enumeratee ByteString [HeapProfile] m a
hpEnum = I.unfoldConvStream hpIter

hpIter :: MonadIO m => HeapProfile -> Iteratee ByteString m (HeapProfile, [HeapProfile])
hpIter = parserToIteratee . hpParse

------------------------------------------------------------
-- The Application
--

hszc :: Application () ()
hszc = def {
          appName = "zoom"
        , appVersion = "0.1"
        , appAuthors = ["Conrad Parker"]
        , appBugEmail = "conrad@metadecks.org"
        , appShortDesc = "Trivial heapscope zoom-cache inspection tools"
        , appLongDesc = longDesc
        , appCategories = ["Reading", "Writing"]
        , appSeeAlso = [""]
        , appProject = "HeapScope"
        , appCmds = [ hszcGen
                    -- , hszcInfo
                    -- , hszcDump
                    -- , hszcSummary
                    ]
	}

longDesc :: String
longDesc = "Manipulate heapscope zoom-cache files"

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain hszc
