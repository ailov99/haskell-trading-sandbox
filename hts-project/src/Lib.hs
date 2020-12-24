{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Lib
    (   readApiKey,
        wreqHello,
        getCurrentPrice
    ) where

import JSONTypes

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as DataText

import System.Environment

import Network.Wreq
import Control.Lens

import Data.Aeson

-- |Reads in the API ket from a system environment variable
readApiKey :: IO String
readApiKey = getEnv "FinnhubApiKey"

-- |Dummy function with a simple wreq GET to test lib is working
wreqHello :: IO ()
wreqHello = do
    response <- get "http://httpbin.org/get"
    -- expecting "application/json"
    ByteString.putStrLn $ response ^. responseHeader "Content-Type"


-- |Query current price for a single stock
getCurrentPrice :: String -> String -> IO ()
getCurrentPrice symbol token = do
    let opts = defaults & param "symbol" .~ [DataText.pack symbol]
                        & param "token" .~ [DataText.pack token]
    response <- asJSON =<< getWith opts "https://finnhub.io/api/v1/quote" :: IO QuoteResp
    putStrLn $ show $ response ^? responseBody
