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
        Right val -> putStrLn $ "Using API Key: " ++ show val

    

