{-# LANGUAGE DeriveGeneric #-}
module Foo where

import GHC.Generics

data Foo = Foo { foo :: String, bar :: Int } deriving (Generic)