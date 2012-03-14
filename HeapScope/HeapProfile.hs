{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module HeapScope.HeapProfile (
    -- * Types
      HPHeader(..)
    , HeapProfile(..)

    -- * Functions
    , emptyHeapProfile
    , emptyHPHeader
    , clearHeapProfile
) where

import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

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

emptyHeapProfile :: HeapProfile
emptyHeapProfile = HeapProfile emptyHPHeader Nothing Nothing Map.empty

emptyHPHeader :: HPHeader
emptyHPHeader = HPHeader "" "" "" ""

clearHeapProfile :: HeapProfile -> HeapProfile
clearHeapProfile hp = hp { hpSampleStart = Nothing, hpSampleEnd = Nothing, hpSamples = Map.empty }

