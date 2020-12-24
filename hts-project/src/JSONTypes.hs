{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module JSONTypes where

import GHC.Generics
import Data.Aeson
import Network.Wreq

-- | Quote
data Quote = Quote {
    o :: Double,
    h :: Double,
    l :: Double,
    c :: Double,
    pc :: Double
} deriving (Generic, Show)

instance ToJSON Quote where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Quote

type QuoteResp = Response (Quote)