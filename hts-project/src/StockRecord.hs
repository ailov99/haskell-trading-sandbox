{-|
Module      : StockRecord
Description : This module contains logic for maintaining the main data structure containing stocks prices (and info)
Stability   : experimental
Portability : POSIX
-}
module StockRecord (
    -- * Functions
    updateStockPrices,
    -- * Types
    StockRecordData(..)
) where

import Lib
import JSONTypes

import qualified StmContainers.Map as STMMap
import GHC.Conc

-- |Master record custom type
type StockRecordData = STMMap.Map String [Double]


-- |Update stock prices
updateStockPrices :: StockRecordData -- ^ Record object to update (ref)
                  -> String          -- ^ Stock symbol to query for
                  -> String          -- ^ API token
                  -> IO ()           -- ^ Context
updateStockPrices recordRef symbol token = do
    -- Fetch price
    maybePriceQuote <- getQuote symbol token

    -- Update record
    case maybePriceQuote of
        Nothing -> do
            putStrLn "updateStockPrices fetch failed..."
            return ()
        Just priceQuote -> do
            -- Add to record 
            atomically $ do
                maybePrices <- STMMap.lookup symbol recordRef
                case maybePrices of 
                    Nothing -> do
                        STMMap.insert [currentPrice :: Double] symbol recordRef
                    Just pricesList -> do
                        STMMap.insert (newElement) symbol recordRef
                        where 
                            newElement = currentPrice:pricesList
            -- DEBUG: print record
            ioPrices <- atomically $ STMMap.lookup symbol recordRef
            putStrLn $ symbol ++ " prices: " ++ show ioPrices
            
            where 
                currentPrice = c priceQuote


