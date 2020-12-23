{-# LANGUAGE OverloadedStrings #-}
module Lib
    (   readApiKey,
        wreqHello,
        getCurrentPrice
    ) where

import qualified Data.ByteString.Char8 as ByteString

import System.Environment

import Network.Wreq
import Control.Lens

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
getCurrentPrice :: IO ()
getCurrentPrice = putStrLn "Pass"