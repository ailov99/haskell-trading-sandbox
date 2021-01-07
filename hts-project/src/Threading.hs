{-|
Module      : Threading
Description : This file contains any logic and tools relevant to concurrent operations
Stability   : experimental
Portability : POSIX
-}
module Threading (
    -- * Functions
    sleepSeconds,
    asyncForkStockPriceUpdates
) where

import Control.Concurrent (threadDelay, ThreadId)
import Control.Monad
import Control.Concurrent (forkIO)

import StockRecord


-- | Thread sleep helper, in seconds
sleepSeconds :: Int   -- ^ Seconds
             -> IO () -- ^ Context
sleepSeconds n = threadDelay $ n * 1000 * 1000


-- | Sync periodic timer
--  Fork it for async
doPeriodically :: Int             -- ^ Delay in seconds
               -> (StockRecordData -> String -> String -> IO ()) -- ^ Action 
               -> StockRecordData -- ^ Storage Record data structure
               -> String          -- ^ Stock Symbol
               -> String          -- ^ API Token
               -> IO ()           -- ^ Context
doPeriodically delaySec f record symbol token =
    forever $ sleepSeconds delaySec >> f record symbol token  


-- | Launch price thread
asyncForkStockPriceUpdates :: StockRecordData -- ^ Storage Record data structure
                           -> Int             -- ^ Periodic delay in seconds
                           -> String          -- ^ Stock Symbol
                           -> String          -- ^ API Token
                           -> IO ThreadId     -- ^ Launched thread ID
asyncForkStockPriceUpdates record delaySec symbol token = do
    -- Bind to actually get the printout, rather than a monadic promise
    threadid <- forkIO $ doPeriodically delaySec updateStockPrices record symbol token
    return threadid
