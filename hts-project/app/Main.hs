module Main where

import System.IO.Error
import Control.Concurrent (forkIO)

import Lib
import Threading


main :: IO ()
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
            _ <- forkIO $ doPeriodically 2 getCurrentPrice "TSLA" token

            -- Testing API ...
            --getCurrentPrice "TSLA" token
            --getCompanyProfile "TSLA" token
            -- This one takes a while ...
            --getSupportedStocks "US" "USD" token

            -- Block for a while
            sleepSeconds 120

            

    

    

