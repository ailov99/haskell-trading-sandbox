{-|
Module      : JSONTypes
Description : This module contains logic for parsing the JSON web responses containing stock market information 
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, TemplateHaskell #-}
module JSONTypes where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import Network.Wreq
import Data.Text
import Control.Monad

-- | Readability type aliases
type URL           = Text
type DateAsText    = Text
type NumAsText     = Text
type UNIXTimestamp = Int
type ID            = Int


-- | Quote
data Quote = Quote {
    o  :: Double,  -- Open
    h  :: Double,  -- High
    l  :: Double,  -- Low
    c  :: Double,  -- Current
    pc :: Double   -- Previous Close
} deriving (Generic, Show)

-- | Quote to JSON
instance ToJSON Quote where
    toEncoding = genericToEncoding defaultOptions

-- | JSON to Quote
instance FromJSON Quote

-- | HTTP Quote response
type QuoteResp = Response (Quote)


-- | Company Profile
data CompanyProfile = CompanyProfile {
    country              :: Text,
    currency             :: Text,
    exchange             :: Text,
    ipo                  :: DateAsText,
    marketCapitalization :: Double,
    name                 :: Text,
    phone                :: NumAsText,
    shareOutstanding     :: Double,
    ticker               :: Text,
    weburl               :: URL,
    logo                 :: URL,
    finnhubIndustry      :: Text
} deriving (Generic, Show)

-- | Company Profile to JSON
instance ToJSON CompanyProfile where
    toEncoding = genericToEncoding defaultOptions

-- | JSON to Company profile
instance FromJSON CompanyProfile

-- | HTTP Company Profile Response
type CompanyProfileResp = Response (CompanyProfile)


-- | Supported Stock
data SupportedStock = SupportedStock {
    currency      :: Text,
    description   :: Text,
    displaySymbol :: Text,
    figi          :: Text,
    mic           :: Text,
    symbol        :: Text,
    stock_type    :: Text
} deriving (Generic, Show)

-- Hack default options as the "type" JSON field clashes with the type keyword...
$(deriveJSON defaultOptions {fieldLabelModifier = \x -> if x == "stock_type" then "type" else x} ''SupportedStock)

-- | Supported Stock Response
type SupportedStockResp = Response ([SupportedStock])


-- |Market News
data MarketNews = MarketNews {
    category :: Text,
    datetime :: UNIXTimestamp,
    headline :: Text,
    id       :: ID,
    image    :: URL,
    related  :: Text,
    source   :: Text,
    summary  :: Text,
    url      :: URL
} deriving (Generic, Show)

-- | Market News to JSON
instance ToJSON MarketNews where
    toEncoding = genericToEncoding defaultOptions

-- | JSON to Market News
instance FromJSON MarketNews

-- | Market News HTTP Response
type MarketNewsResp = Response ([MarketNews])


-- | News Sentiment and stats for a company
data CompanyNewsSentiment = CompanyNewsSentiment {
    buzz                        :: CompanyNewsStats,
    companyNewsScore            :: Double,
    sectorAverageBullishPercent :: Double,
    sectorAverageNewsScore      :: Double,
    sentiment                   :: NewsSentiment,
    symbol                      :: Text
} deriving (Generic, Show)

-- | Company News Sentiment to JSON
instance ToJSON CompanyNewsSentiment where
    toEncoding = genericToEncoding defaultOptions

-- | JSON to Company News Sentiment
instance FromJSON CompanyNewsSentiment

-- | Company News Sentiment HTTP Response
type CompanyNewsSentimentResp = Response (CompanyNewsSentiment)


-- | Company News
data CompanyNewsStats = CompanyNewsStats {
    articlesInLastWeek :: Int,
    buzz             :: Double,
    weeklyAverage    :: Double
} deriving (Generic, Show)

-- | Company News to JSON
instance ToJSON CompanyNewsStats where
    toEncoding = genericToEncoding defaultOptions

-- | JSON to Company News
instance FromJSON CompanyNewsStats


-- | News Sentiment
data NewsSentiment = NewsSentiment {
    bearishPercent :: Double,
    bullishPercent :: Double
} deriving (Generic, Show) 

-- | News Sentiment to JSON
instance ToJSON NewsSentiment where
    toEncoding = genericToEncoding defaultOptions

-- | JSON to News Sentiment
instance FromJSON NewsSentiment