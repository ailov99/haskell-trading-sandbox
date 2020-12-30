{-# LANGUAGE OverloadedStrings, DeriveDataTypeable  #-}
module Lib (
    readApiKey,
    wreqHello,
    getQuote,
    getCompanyProfile,
    getSupportedStocks,
    getMarketNews,
    getCompanyNews,
    MarketNewsCategory (..),
    FormattedDate (..)
) where

import JSONTypes

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as DataText

import System.Environment

import Network.Wreq
import Control.Lens

import Data.Aeson
import Data.Data

-- | ------------ Enumerated types ------
data MarketNewsCategory = General | Forex | Crypto | Merger deriving (Eq, Show, Data, Typeable)
parseMarketNewsCategory :: MarketNewsCategory -> String 
parseMarketNewsCategory category = case category of
    General -> "general"
    Forex   -> "forex"
    Crypto  -> "crypto"
    Merger  -> "merger"

-- | ------------ Custom types ----------
data FormattedDate = FormattedDate {
    day   :: Int,
    month :: Int,
    year  :: Int
} deriving (Eq)

instance Show FormattedDate where
    show (FormattedDate d m y) = 
        show y ++ "-" ++ 
        if m < 10 
            then "0" ++ show m 
            else show m 
            ++ "-" ++ 
            if d < 10
                then "0" ++ show d
                else show d

instance Ord FormattedDate where
    compare (FormattedDate d m y) (FormattedDate d' m' y') =
        if y /= y' 
            then compare y y'
            else if m /= m' 
                then compare m m'
                else compare d d'

-- | ------------ URLs ------------------
testGetURL            = "http://httpbin.org/get"
finnhubQuoteURL       = "https://finnhub.io/api/v1/quote"
finnhubProfileURL     = "https://finnhub.io/api/v1/stock/profile2"
finnhubSuppStocksURL  = "https://finnhub.io/api/v1/stock/symbol"
finnhubMarketNewsURL  = "https://finnhub.io/api/v1/news"
finnhubCompanyNewsURL = "https://finnhub.io/api/v1/company-news"

-- | ------------ API -------------------

-- |Reads in the API ket from a system environment variable
readApiKey :: IO String
readApiKey = getEnv "FinnhubApiKey"


-- |Dummy function with a simple wreq GET to test lib is working
wreqHello :: IO ()
wreqHello = do
    response <- get testGetURL
    -- expecting "application/json"
    ByteString.putStrLn $ response ^. responseHeader "Content-Type"


-- |Get a Quote for a single stock
-- API doc: https://finnhub.io/docs/api#quote
getQuote :: String -> String -> IO ()
getQuote symbol token = do
    let opts = defaults & param "symbol" .~ [DataText.pack symbol]
                        & param "token"  .~ [DataText.pack token]
    response <- asJSON =<< getWith opts finnhubQuoteURL :: IO QuoteResp
    putStrLn $ show $ response ^? responseBody


-- |Query general profile of a company
-- API doc: https://finnhub.io/docs/api#company-profile2
getCompanyProfile :: String -> String -> IO ()
getCompanyProfile symbol token = do
    let opts = defaults & param "symbol" .~ [DataText.pack symbol]
                        & param "token"  .~ [DataText.pack token]
    response <- asJSON =<< getWith opts finnhubProfileURL :: IO CompanyProfileResp
    putStrLn $ show $ response ^? responseBody    


-- |Query for all supported stocks
-- API doc: https://finnhub.io/docs/api#stock-symbols
getSupportedStocks :: String -> String -> String -> IO ()
getSupportedStocks exchange currency token = do
    let opts = defaults & param "exchange" .~ [DataText.pack exchange]
                        & param "currency" .~ [DataText.pack currency]
                        & param "token"    .~ [DataText.pack token]
    response <- asJSON =<< getWith opts finnhubSuppStocksURL :: IO SupportedStockResp
    putStrLn $ show $ response ^? responseBody


-- |Get latest market news
-- API doc: https://finnhub.io/docs/api#market-news
getMarketNews :: MarketNewsCategory -> String -> IO ()
getMarketNews category token = do
    let categoryString = parseMarketNewsCategory category
    let opts = defaults & param "category" .~ [DataText.pack categoryString]
                        & param "token"    .~ [DataText.pack token]
    response <- asJSON =<< getWith opts finnhubMarketNewsURL :: IO MarketNewsResp
    putStrLn $ show $ response ^? responseBody

-- |Get company news for a given period
-- Note: This only works for NA companies
-- API doc: https://finnhub.io/docs/api#company-news
getCompanyNews :: String -> FormattedDate -> FormattedDate -> String -> IO ()
getCompanyNews symbol fromDate toDate token = do
    let fromDateStr = show fromDate
    let toDateStr = show toDate
    getCompanyNewsStr symbol fromDateStr toDateStr token

getCompanyNewsStr :: String -> String -> String -> String -> IO ()
getCompanyNewsStr symbol fromDate toDate token = do
    let opts = defaults & param "symbol" .~ [DataText.pack symbol]
                        & param "from"   .~ [DataText.pack fromDate]
                        & param "to"     .~ [DataText.pack toDate]
                        & param "token"  .~ [DataText.pack token]
    response <- asJSON =<< getWith opts finnhubCompanyNewsURL :: IO MarketNewsResp
    putStrLn $ show $ response ^? responseBody


