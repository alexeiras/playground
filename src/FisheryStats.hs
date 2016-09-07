{-# LANGUAGE OverloadedStrings #-}

module FisheryStats
    (
    -- * Dataset query and transformation functions
      range
    , rangeMulti
    , toSalesLines
    -- * Utilities
    , fromRight
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as S
import           Data.Function
import           Data.Time
import           Database.Redis
import           Playground.Types

dbConn :: IO Connection
dbConn = connect defaultConnectInfo { connectHost = "192.168.99.100" }

range :: (RedisKey k) => k -> Bounds -> IO (Either Reply [S.ByteString])
range k bs = do
    conn <- dbConn
    runRedis conn $ redisCmd (key k) bs
        where
            redisCmd k (s :/: s') = zrangebyscore k (score s) (score s')
            redisCmd k (i :|: i') = zrange k i i'

rangeMulti :: (RedisKey k) => [k] -> Bounds -> IO [Either Reply [S.ByteString]]
rangeMulti ks bs = sequence $ fmap (bs & flip range) ks

toSalesLines :: Either Reply [S.ByteString] -> [SalesLine]
toSalesLines = map toSalesLine . fromRight

toSalesLine :: S.ByteString -> SalesLine
toSalesLine = fromRight . parseOnly parseSalesLine

parseSalesLine :: Parser SalesLine
parseSalesLine = do
    date <- parseLocalTime <* char ':'
    amount <- parseFractional <* char ':'
    price <- parseFractional <* char ':'
    minPrice <- parseFractional <* char ':'
    maxPrice <- parseFractional <* char ':'
    avgPrice <- parseFractional
    return $ SalesLine date amount price minPrice maxPrice avgPrice

parseLocalTime :: Parser LocalTime
parseLocalTime = do
    str <- takeTill (== ':')
    parseTimeM False defaultTimeLocale "%-d/%-m/%Y" $ S.unpack str

parseFractional :: Parser Double
parseFractional = do
    i <- concat <$> many digit `sepBy` char '.'
    f <- option "0" ("," *> many digit)
    return $ read $ i ++ "." ++ f

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight: Left"
