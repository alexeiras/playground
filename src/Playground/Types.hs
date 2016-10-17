{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Playground.Types
    ( Params(..)
    , Bounds(..)
    , SalesLine(..)
    , RedisKey(..)
    , Score(..)
    ) where

import qualified Data.ByteString.Char8 as S
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock.POSIX

data Params
    = Species { speciesId :: S.ByteString }
    | Markets { marketId :: S.ByteString }
    | Sales { species :: S.ByteString , market :: S.ByteString }

data Bounds where
    (:/:) :: (Score s) => s -> s -> Bounds
    (:|:) :: Integer -> Integer -> Bounds

data SalesLine = SalesLine
    { date     :: LocalTime
    , amount   :: Double
    , price    :: Double
    , minPrice :: Double
    , maxPrice :: Double
    , avgPrice :: Double
    } deriving (Show)

class RedisKey k where
    key :: k -> S.ByteString

instance RedisKey Params where
    key Sales{..}   = S.intercalate ":" ["sales", species, market]
    key (Species _) = "species"
    key (Markets _) = "markets"

instance RedisKey S.ByteString where
    key = id

instance RedisKey String where
    key = S.pack

class (Show s) => Score s where
    score :: s -> Double

instance Score UTCTime where
    score = realToFrac . utcTimeToPOSIXSeconds

instance Score String where
    score = score . fromJust . parseDate
        where
            parseDate :: String -> Maybe UTCTime
            parseDate = parseTimeM False defaultTimeLocale "%-d/%-m/%Y"
