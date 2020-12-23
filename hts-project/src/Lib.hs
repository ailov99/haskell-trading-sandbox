{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( wreqHello
    ) where

import qualified Data.ByteString.Char8 as ByteString
import Network.Wreq
import Control.Lens

wreqHello :: IO ()
wreqHello = do
    response <- get "http://httpbin.org/get"
    ByteString.putStrLn $ response ^. responseHeader "Content-Type"