{-|
Module      : Main
Description : Main driver module
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import System.IO.Error
import Control.Concurrent (forkIO)
import Text.RawString.QQ

import Lib
import Threading
import StockRecord

import qualified StmContainers.Map as STMMap
import GHC.Conc


-- | User menu string
userMenuString :: String
userMenuString = [r|
Please select an option:
q - quit
|]


-- | User input loop
userInputLoop :: IO () -- ^ Context
userInputLoop = do
    putStrLn userMenuString
    userIn <- getLine

    case userIn of
        "q" -> return ()
        _   -> userInputLoop 



-- | Main driver
main :: IO () -- ^ Context
main = do
    -- Read in the API key from the local system
    apiKey <- tryIOError readApiKey
    case apiKey of

        Left err -> 
            if isDoesNotExistError err
                then do
                    putStrLn "API key not found. Please ensure an env var FinnhubApiKey is defined."
                    return ()
                else ioError err
        
        Right token -> do
            -- If the read was good, use they key for quering the web
            putStrLn $ "Using API Key: " ++ show token

            -- Threading
            --aMap <- STMMap.newIO
            --_ <- asyncForkStockPriceUpdates aMap 5 "TSLA" token
            --_ <- asyncForkStockPriceUpdates aMap 3 "GME" token
            --let tickerSymbols = ["TSLA", "GME"]
            --launchAsyncWorkersForTickerSymbols aMap 5 tickerSymbols token

            userInputLoop

            -- Shut down in 10 seconds
            putStr "Shutting down"
            shutDown 10

            

    

    

