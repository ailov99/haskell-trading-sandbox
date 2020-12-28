module Threading (
    doPeriodically,
    sleepSeconds
) where

import Control.Concurrent (threadDelay)
import Control.Monad


-- |Thread sleep helper, in seconds
sleepSeconds :: Int -> IO ()
sleepSeconds n = threadDelay $ n * 1000 * 1000

-- |Sync periodic timer
--  Fork it for async
doPeriodically :: Int -> (String -> String -> IO ()) -> String -> String -> IO ()
doPeriodically delaySec f symbol token =
    forever $ sleepSeconds delaySec >> f symbol token  