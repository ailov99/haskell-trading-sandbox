{-|
Module      : Main
Description : Main driver module
Stability   : experimental
Portability : POSIX
-}
module Main where

import System.IO.Error
import Control.Concurrent (forkIO)

import Lib
import Threading
import StockRecord

import qualified StmContainers.Map as STMMap
import GHC.Conc

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
            aMap <- STMMap.newIO
            _ <- asyncForkStockPriceUpdates aMap 5 "TSLA" token

            -- Block for a while
            sleepSeconds 120

            

    

    

