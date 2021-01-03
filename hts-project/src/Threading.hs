{-|
  Threading.hs
  This file contains any logic and tools relevant to concurrent operations
-}
module Threading (
    sleepSeconds,
    asyncForkStockPriceUpdates
) where

import Control.Concurrent (threadDelay, ThreadId)
import Control.Monad
import Control.Concurrent (forkIO)

import StockRecord


-- |Thread sleep helper, in seconds
sleepSeconds :: Int -> IO ()
sleepSeconds n = threadDelay $ n * 1000 * 1000


-- |Sync periodic timer
--  Fork it for async
doPeriodically :: Int -> (StockRecordData -> String -> String -> IO ()) -> StockRecordData -> String -> String -> IO ()
doPeriodically delaySec f record symbol token =
    forever $ sleepSeconds delaySec >> f record symbol token  


-- |Launch price thread
asyncForkStockPriceUpdates :: StockRecordData -> Int -> String -> String -> IO ThreadId
asyncForkStockPriceUpdates record delaySec symbol token = do
    -- Bind to actually get the printout, rather than a monadic promise
    threadid <- forkIO $ doPeriodically delaySec updateStockPrices record symbol token
    return threadid
