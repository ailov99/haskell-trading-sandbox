{-# LANGUAGE OverloadedStrings #-}
module Lib (
    readApiKey,
    wreqHello,
    getCurrentPrice,
    getCompanyProfile,
    getSupportedStocks
) where

import JSONTypes

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as DataText

import System.Environment

import Network.Wreq
import Control.Lens

import Data.Aeson


-- | ------------ URLs ------------------
testGetURL           = "http://httpbin.org/get"
finnhubQuoteURL      = "https://finnhub.io/api/v1/quote"
finnhubProfileURL    = "https://finnhub.io/api/v1/stock/profile2"
finnhubSuppStocksURL = "https://finnhub.io/api/v1/stock/symbol"


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


-- |Query current price for a single stock
getCurrentPrice :: String -> String -> IO ()
getCurrentPrice symbol token = do
    let opts = defaults & param "symbol" .~ [DataText.pack symbol]
                        & param "token"  .~ [DataText.pack token]
    response <- asJSON =<< getWith opts finnhubQuoteURL :: IO QuoteResp
    putStrLn $ show $ response ^? responseBody


-- |Query general profile of a company
getCompanyProfile :: String -> String -> IO ()
getCompanyProfile symbol token = do
    let opts = defaults & param "symbol" .~ [DataText.pack symbol]
                        & param "token"  .~ [DataText.pack token]
    response <- asJSON =<< getWith opts finnhubProfileURL :: IO CompanyProfileResp
    putStrLn $ show $ response ^? responseBody    


-- |Query for all supported stocks
getSupportedStocks :: String -> String -> String -> IO ()
getSupportedStocks exchange currency token = do
    let opts = defaults & param "exchange" .~ [DataText.pack exchange]
                        & param "currency" .~ [DataText.pack currency]
                        & param "token"    .~ [DataText.pack token]
    response <- asJSON =<< getWith opts finnhubSuppStocksURL :: IO SupportedStockResp
    putStrLn $ show $ response ^? responseBody
