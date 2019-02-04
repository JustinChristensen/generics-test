{-# LANGUAGE DeriveGeneric #-}
module Bar where

import GHC.Generics

data Bar = Bar { bar :: Bool, foo :: String } deriving (Generic)