{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, TemplateHaskell #-}
module JSONTypes where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import Network.Wreq
import Data.Text
import Control.Monad


type URL           = Text
type DateAsText    = Text
type NumAsText     = Text
type UNIXTimestamp = Int
type ID            = Int


-- |Quote
data Quote = Quote {
    o  :: Double,  -- Open
    h  :: Double,  -- High
    l  :: Double,  -- Low
    c  :: Double,  -- Current
    pc :: Double   -- Previous Close
} deriving (Generic, Show)

instance ToJSON Quote where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Quote

type QuoteResp = Response (Quote)


-- |Company Profile
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

instance ToJSON CompanyProfile where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CompanyProfile

type CompanyProfileResp = Response (CompanyProfile)


-- |Supported Stock
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

instance ToJSON MarketNews where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON MarketNews

type MarketNewsResp = Response ([MarketNews])