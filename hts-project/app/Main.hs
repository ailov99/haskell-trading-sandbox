module Main where

import System.IO.Error

import Lib

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
        
        Right val -> do
            -- If the read was good, use they key for quering the web
            putStrLn $ "Using API Key: " ++ show val
            getCurrentPrice "TSLA" val
            getCompanyProfile "TSLA" val
            getSupportedStocks "US" "USD" val

    

    

