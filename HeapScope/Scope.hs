{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : HeapScope.Scope
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

   HeapScope plotting functions
-}

module HeapScope.Scope (
      scopeReadHeapProfile
    , hsIdentifiers
) where

import Control.Arrow (second)
import qualified Data.Foldable as Fold
import Data.Maybe (fromJust)
import Data.ZoomCache
import Data.ZoomCache.Codec

import Scope.Types hiding (b)

import Data.Iteratee.ZoomCache.HeapProfile
import HeapScope.HeapProfile
import HeapScope.ZoomCache

----------------------------------------------------------------------

instance ScopePlot HeapProfile where
    rawLayerPlot = rawLayerPlotListHeapProfile
    summaryLayerPlot = summaryLayerPlotListHeapProfile

----------------------------------------------------------------------

-- | ScopeRead methods to interpret numeric data as HeapProfile
scopeReadHeapProfile :: ScopeRead
scopeReadHeapProfile = ScopeRead (ReadMethods hsIdentifiers extentsHeapProfile enumListHeapProfile (enumSummaryListHeapProfile 1))

hsIdentifiers :: [IdentifyCodec]
hsIdentifiers = [ identifyCodec (undefined :: HeapProfile) ]

----------------------------------------------------------------------
-- Raw data

rawLayerPlotListHeapProfile :: LayerExtents -> RGB -> LayerPlot (TimeStamp, [HeapProfile])
rawLayerPlotListHeapProfile LayerExtents{..} _rgb =
    LayerFold (plotRawListHeapProfile rangeY) plotRawListInitHeapProfile Nothing

plotRawListInitHeapProfile :: [DrawLayer]
plotRawListInitHeapProfile = repeat []

plotRawListHeapProfile :: Double -> LayerFoldFunc (TimeStamp, [HeapProfile]) (Maybe [HeapProfile])
plotRawListHeapProfile yRange x w Nothing (ts, ys) =
    plotRawListHeapProfile yRange x w (Just ys) (ts, ys)
plotRawListHeapProfile yRange x w (Just ys0) (ts, ys) =
    second Just $ foldl c ([], []) $ map f (zip3 (map yFunc [0..]) ys0 ys)
    where
        c :: ([a], [b]) -> ([a], b) -> ([a], [b])
        c (ds0, ss) (ds, s) = (ds0++ds, ss++[s])

        l = length ys
        yStep = 2.0 / fromIntegral l
        yFunc :: Double -> Double -> Double
        yFunc n v = (-1.0) + (n * yStep) + ((0.5) * yStep) + (v * yStep / yRange)
        f :: ((Double -> Double), HeapProfile, HeapProfile) -> ([DrawLayer], HeapProfile)
        f (y, s0, s) = second fromJust $ plotRaw1HeapProfile y x w (Just s0) (ts, s)

plotRaw1HeapProfile :: (Double -> Double) -> LayerFoldFunc (TimeStamp, HeapProfile) (Maybe HeapProfile)
plotRaw1HeapProfile f x w Nothing (ts, hp) = plotRaw1HeapProfile f x w (Just hp) (ts, hp)
plotRaw1HeapProfile f x w (Just hp0) (_ts, hp) = (cmds, Just hp)
    where
        cmds =
            [ [ MoveTo (x,   y hp0)
              , LineTo (x+w, y hp)
              ]
            ]
        y = f . fromIntegral . Fold.sum . hpSamples

----------------------------------------------------------------------
-- Summary data

summaryLayerPlotListHeapProfile :: LayerExtents -> RGB -> LayerPlot [Summary HeapProfile]
summaryLayerPlotListHeapProfile LayerExtents{..} rgb =
    LayerFold (plotSummaryListHeapProfile rangeY) (plotSummaryListInitHeapProfile rgb) Nothing

plotSummaryListInitHeapProfile :: RGB -> [DrawLayer]
plotSummaryListInitHeapProfile (r, g, b) = concat $ repeat
    [ [ SetRGBA r g b 0.3 ]
    , [ SetRGB (r*0.6) (g*0.6) (b*0.6) ]
    ]

plotSummaryListHeapProfile :: Double
                           -> LayerFoldFunc [Summary HeapProfile] (Maybe [Summary HeapProfile])
plotSummaryListHeapProfile dYRange x w Nothing ss =
    plotSummaryListHeapProfile dYRange x w (Just ss) ss
plotSummaryListHeapProfile dYRange x w (Just ss0) ss = do
    second Just $ foldl c ([], []) $ map f (zip3 (map yFunc [0..]) ss0 ss)
    where
        c :: ([a], [b]) -> ([a], b) -> ([a], [b])
        c (ds0, sss) (ds, s) = (ds0++ds, sss++[s])

        l = length ss
        yStep = 2.0 / fromIntegral l
        yFunc :: Double -> Double -> Double
        yFunc n v = (-1.0) + (n * yStep) + ((0.5) * yStep) + (v * yStep / dYRange)
        f :: ((Double -> Double), Summary HeapProfile, Summary HeapProfile) -> ([DrawLayer], Summary HeapProfile)
        f (y, s0, s) = second fromJust $ plotSummary1HeapProfile y x w (Just s0) s

-- | Plot one numeric summary
plotSummary1HeapProfile :: (Double -> Double)
                        -> LayerFoldFunc (Summary HeapProfile) (Maybe (Summary HeapProfile))
plotSummary1HeapProfile y x w Nothing s =
    plotSummary1HeapProfile y x w (Just s) s
plotSummary1HeapProfile y x w (Just s0) s = (cmds, Just s)
    where
        cmds =
{-
            [ [ FillPoly [ (x,     y (numMax sd0))
                         , ((x+w), y (numMax sd))
                         , ((x+w), y (numMin sd))
                         , (x,     y (numMin sd0))
                         ]
              ]
-}
            [ [ MoveTo (x,     y (fromIntegral $ summaryHPTotal sd0))
              , LineTo ((x+w), y (fromIntegral $ summaryHPTotal sd))
              ]
            ]
        sd0 = summaryData s0
        sd = summaryData s

