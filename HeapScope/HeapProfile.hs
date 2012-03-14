{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

module HeapScope.HeapProfile (
    -- * Types
      HPHeader(..)
    , HeapProfile(..)
) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Typeable

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

